;;; -*- lexical-binding: t; -*-
;;; test-transport.el --- Layer 1 transport tests (SSH localhost)

;; These tests exercise the full client/server pipe end-to-end against
;; `ssh localhost'.  They require passwordless `ssh localhost' to be
;; configured (key-based auth, no password prompt).

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)

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

;;; test-transport.el ends here
