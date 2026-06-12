;;; -*- lexical-binding: t; -*-
;;; test-transport.el --- Layer 1 transport tests (SSH localhost)

;; These tests exercise the full client/server pipe end-to-end against
;; `ssh localhost'.  They require passwordless `ssh localhost' to be
;; configured (key-based auth, no password prompt).

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)

(defconst angelia-tests--target-host "localhost"
  "Single-VM test target.  Single point to retarget all transport tests.")

(defun angelia-tests--remote-server-path ()
  "Absolute path of the deployed server.el on the test target."
  (expand-file-name "~/.cache/angelia/server.el"))

(defun angelia-tests--purge-remote-cache ()
  "Delete the deployed server.el so the next connect must re-upload."
  (let ((p (angelia-tests--remote-server-path)))
    (when (file-exists-p p) (delete-file p))))

;; Run before each test in this file to make sure no leaked connection from a
;; prior test changes the outcome of the next one.
(defun angelia-tests--transport-setup ()
  (angelia-tests-ensure-no-connections))

;; ---------------------------------------------------------------------------

(ert-deftest test-connection-lifecycle ()
  "Connecting yields a live process; disconnecting drops it from the registry."
  (angelia-tests--transport-setup)
  (let (conn)
    (unwind-protect
        (progn
          (setq conn (angelia-client-connect angelia-tests--target-host))
          (should (angelia-client--conn-p conn))
          (should (process-live-p (angelia-client--conn-process conn)))
          (should (gethash angelia-tests--target-host
                           angelia-client--connections))
          (angelia-client-disconnect angelia-tests--target-host)
          (should-not (gethash angelia-tests--target-host
                               angelia-client--connections))
          (should-not (process-live-p (angelia-client--conn-process conn))))
      (when (gethash angelia-tests--target-host angelia-client--connections)
        (angelia-client-disconnect angelia-tests--target-host)))))

(ert-deftest test-ping-pong ()
  "`server/ping' round-trips and stays under one second."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((t0 (current-time))
           (resp (angelia-client-call angelia-tests--target-host
                                      'server/ping nil))
           (elapsed (float-time (time-subtract (current-time) t0))))
      (should (eq (plist-get resp :pong) t))
      (should (stringp (plist-get resp :timestamp)))
      (should (< elapsed 1.0)))))

(ert-deftest test-version-handshake ()
  "`server/version' returns the SHA1 the client embeds.  Mismatch makes
`angelia-client-connect' signal `angelia-client-version-mismatch' before
returning, so reaching this body proves the handshake passed."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let ((resp (angelia-client-call angelia-tests--target-host
                                     'server/version nil)))
      (should (equal (plist-get resp :sha1) angelia-client--server-sha1))
      (should (stringp (plist-get resp :emacs_version)))
      (should (integerp (plist-get resp :pid))))))

(ert-deftest test-deploy-and-launch ()
  "Full bootstrap: blow away the remote cache, connect, verify the server is
up and reports the freshly-uploaded SHA1."
  (angelia-tests--transport-setup)
  (angelia-tests--purge-remote-cache)
  (should-not (file-exists-p (angelia-tests--remote-server-path)))
  (with-angelia-connection angelia-tests--target-host conn
    (should (process-live-p (angelia-client--conn-process conn)))
    ;; File should now exist on the remote with the expected SHA1.
    (should (file-exists-p (angelia-tests--remote-server-path)))
    (let ((info (angelia-client-call angelia-tests--target-host
                                     'server/info nil)))
      (should (equal (plist-get info :sha1) angelia-client--server-sha1)))))

(ert-deftest test-reconnect ()
  "Disconnect then re-connect produces a fresh, healthy connection."
  (angelia-tests--transport-setup)
  (let (first-pid second-pid)
    (unwind-protect
        (progn
          (let ((conn (angelia-client-connect angelia-tests--target-host)))
            (setq first-pid (process-id
                             (angelia-client--conn-process conn)))
            (should (process-live-p (angelia-client--conn-process conn))))
          (angelia-client-disconnect angelia-tests--target-host)
          (should-not (gethash angelia-tests--target-host
                               angelia-client--connections))
          (let ((conn (angelia-client-connect angelia-tests--target-host)))
            (setq second-pid (process-id
                              (angelia-client--conn-process conn)))
            (should (process-live-p (angelia-client--conn-process conn)))
            ;; Different process this time around.
            (should-not (equal first-pid second-pid))
            ;; And it still answers ping.
            (should (eq (plist-get (angelia-client-call
                                    angelia-tests--target-host
                                    'server/ping nil)
                                   :pong)
                        t))))
      (when (gethash angelia-tests--target-host angelia-client--connections)
        (angelia-client-disconnect angelia-tests--target-host)))))

(ert-deftest test-concurrent-requests ()
  "Five concurrent async `server/ping' requests all complete."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let ((results (make-vector 5 nil))
          (errors  (make-vector 5 nil))
          (done 0))
      (dotimes (k 5)
        ;; Fresh per-iteration binding so each closure captures its own
        ;; index.  `dotimes' re-uses the same binding by default.
        (let ((idx k))
          (angelia-client-async
           angelia-tests--target-host 'server/ping nil
           (lambda (resp)
             (aset results idx resp)
             (cl-incf done))
           (lambda (err)
             (aset errors idx err)
             (cl-incf done)))))
      (with-timeout (5 (error "Timed out: only %d/5 responses arrived" done))
        (while (< done 5)
          (accept-process-output nil 0.1)))
      (dotimes (k 5)
        (should (null (aref errors k)))
        (should (eq (plist-get (aref results k) :pong) t))))))

;; ---------------------------------------------------------------------------
;; Phase 1 -- sessions over the SSH pipe.

(defun angelia-tests--echo-params (count payload &optional end-immediately)
  "Build a session/echo params hash."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "count" count p)
    (puthash "payload" payload p)
    (when end-immediately (puthash "end-immediately" t p))
    p))

(ert-deftest test-session-open-and-close ()
  "Open a session/echo, receive N events + end, verify callback unregistered."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host conn
    (let* ((events '())
           (ended nil)
           (session
            (angelia-client-open-session
             angelia-tests--target-host 'session/echo
             (angelia-tests--echo-params 3 "alpha")
             (lambda (_kind params)
               (push (plist-get params :index) events))
             :on-end (lambda (_p) (setq ended t)))))
      (should (stringp session))
      ;; Session is registered locally until end arrives.
      (should (gethash session (angelia-client--conn-sessions conn)))
      (with-timeout (3 (error "session never ended"))
        (while (not ended)
          (accept-process-output nil 0.05)))
      (should (equal (nreverse events) '(0 1 2)))
      ;; After end, the callback row must be gone.
      (should-not (gethash session (angelia-client--conn-sessions conn))))))

(ert-deftest test-session-concurrent ()
  "Three concurrent session/echo streams interleave correctly; each callback
sees only its own session's events in order."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((indices (make-vector 3 nil))
           (ends (make-vector 3 nil))
           (sessions (make-vector 3 nil)))
      (dotimes (k 3)
        (let ((slot k))
          (aset sessions slot
                (angelia-client-open-session
                 angelia-tests--target-host 'session/echo
                 (angelia-tests--echo-params 5 (format "marker-%d" slot))
                 (lambda (_kind params)
                   ;; Sanity: payload must match this slot.
                   (should (equal (plist-get params :payload)
                                  (format "marker-%d" slot)))
                   (push (plist-get params :index) (aref indices slot)))
                 :on-end (lambda (_p) (aset ends slot t))))))
      (with-timeout (5 (error "not all sessions ended: %S" ends))
        (while (not (and (aref ends 0) (aref ends 1) (aref ends 2)))
          (accept-process-output nil 0.05)))
      (dotimes (k 3)
        (should (equal (nreverse (aref indices k)) '(0 1 2 3 4))))
      ;; The three session ids must be distinct.
      (should (= 3 (length (delete-dups (append sessions nil))))))))

(ert-deftest test-session-server-side-close ()
  "When the server ends a session immediately, `on-end' fires with no events."
  (angelia-tests--transport-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((events '())
           (ended nil))
      (angelia-client-open-session
       angelia-tests--target-host 'session/echo
       (angelia-tests--echo-params 99 "should-not-arrive" t)
       (lambda (_kind params) (push params events))
       :on-end (lambda (_p) (setq ended t)))
      (with-timeout (3 (error "on-end never fired"))
        (while (not ended)
          (accept-process-output nil 0.05)))
      (should ended)
      (should (null events)))))

;; ---------------------------------------------------------------------------
;; Auto-reconnect (Step 3).  `angelia-client-reconnect-max-attempts' is set to 0
;; in these tests to disable the background-timer path and isolate the
;; deterministic call-time reconnect.

(ert-deftest test-auto-reconnect-on-call ()
  "After the ssh process is killed, the next `angelia-client-call' transparently
reconnects and succeeds on a genuinely new process."
  (angelia-tests--transport-setup)
  (let ((angelia-client-auto-reconnect t)
        (angelia-client-reconnect-max-attempts 0))
    (unwind-protect
        (let* ((conn (angelia-client-connect angelia-tests--target-host))
               (first-pid (process-id (angelia-client--conn-process conn))))
          (should (process-live-p (angelia-client--conn-process conn)))
          (delete-process (angelia-client--conn-process conn))
          (should-not (process-live-p (angelia-client--conn-process conn)))
          ;; The next call must succeed by reconnecting under the hood.
          (should (eq (plist-get (angelia-client-call
                                  angelia-tests--target-host 'server/ping nil)
                                 :pong)
                      t))
          (let ((new (angelia-client--existing-live-connection
                      angelia-tests--target-host)))
            (should new)
            (should-not (equal first-pid
                               (process-id (angelia-client--conn-process new))))))
      (when (gethash angelia-tests--target-host angelia-client--connections)
        (angelia-client-disconnect angelia-tests--target-host)))))

(ert-deftest test-connect-survives-failing-after-connect-hook ()
  "An error in one after-connect hook neither fails connect nor skips the rest.
The hook variable is a public extension point; before the fix a signalling
hook bubbled into connect's cleanup branch, which tore down the live
connection it had just established."
  (angelia-tests--transport-setup)
  (let* ((later-ran nil)
         (angelia-client-after-connect-functions
          (append angelia-client-after-connect-functions
                  (list (lambda (_h) (error "deliberate hook failure"))
                        (lambda (_h) (setq later-ran t))))))
    (with-angelia-connection angelia-tests--target-host conn
      (should conn)
      (should (angelia-client--existing-live-connection
               angelia-tests--target-host))
      ;; The hook after the failing one still ran.
      (should later-ran)
      ;; And the connection is genuinely usable.
      (should (eq t (plist-get (angelia-client-call
                                angelia-tests--target-host 'server/ping nil)
                               :pong))))))

(ert-deftest test-explicit-disconnect-no-auto-reconnect ()
  "An explicit `angelia-client-disconnect' is never auto-reconnected."
  (angelia-tests--transport-setup)
  (let ((angelia-client-auto-reconnect t)
        (angelia-client-reconnect-max-attempts 5)
        (angelia-client-reconnect-base-delay 0.1))
    (angelia-client-connect angelia-tests--target-host)
    (angelia-client-disconnect angelia-tests--target-host)
    ;; Pump well past the first backoff window; the host must stay down.
    (with-timeout (1 nil)
      (while t (accept-process-output nil 0.1)))
    (should-not (gethash angelia-tests--target-host
                         angelia-client--connections))))

(ert-deftest test-watch-survives-reconnect ()
  "A file-notify watch is re-registered (same descriptor, fresh session) after a
dropped connection is restored."
  (angelia-tests--transport-setup)
  (let ((angelia-client-auto-reconnect t)
        (angelia-client-reconnect-max-attempts 0)
        (dir (file-name-as-directory (make-temp-file "angelia-recon-watch-" t))))
    (unwind-protect
        (progn
          (angelia-client-connect angelia-tests--target-host)
          (let* ((url (concat "/@angelia:" angelia-tests--target-host ":" dir))
                 (desc (file-notify-add-watch url '(change) #'ignore))
                 (old-session (plist-get
                               (gethash desc angelia-client-files--watches)
                               :session)))
            (should (file-notify-valid-p desc))
            (should old-session)
            ;; Kill the link.
            (delete-process
             (angelia-client--conn-process
              (angelia-client--existing-live-connection
               angelia-tests--target-host)))
            ;; A call reconnects; the after-connect hook re-registers watches.
            (angelia-client-call angelia-tests--target-host 'server/ping nil)
            (should (file-notify-valid-p desc))
            (let ((new-session (plist-get
                                (gethash desc angelia-client-files--watches)
                                :session)))
              (should new-session)
              (should-not (equal old-session new-session)))
            (file-notify-rm-watch desc)))
      (when (gethash angelia-tests--target-host angelia-client--connections)
        (angelia-client-disconnect angelia-tests--target-host))
      (when (file-directory-p dir) (delete-directory dir t)))))

;;; test-transport.el ends here
