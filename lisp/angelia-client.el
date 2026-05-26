;;; -*- lexical-binding: t; -*-
;;; angelia-client.el --- Local client lifecycle for Angelia

;; The local entry point: spawn ssh, wrap its pipes with
;; `jsonrpc-process-connection', handshake against the deployed remote server,
;; and expose a tiny sync/async RPC surface plus the interactive commands
;; (`angelia-client-send-ping' etc.).  File operations live in
;; `angelia-client-files.el' (Step 7).
;;
;; Connections are keyed by HOST string and stored in
;; `angelia-client--connections'.  HOST is whatever you'd pass to `ssh' on the
;; command line -- jump hosts, port specs, `~/.ssh/config' aliases all work
;; transparently because we never parse the string locally.

(require 'cl-lib)
(require 'subr-x)
(require 'jsonrpc)
(require 'angelia-client-deploy)

;; ---------------------------------------------------------------------------
;; Errors + connection record.

(define-error 'angelia-client-version-mismatch
  "Angelia server SHA1 does not match the bundled client copy")

(define-error 'angelia-client-not-connected
  "No active Angelia connection for the requested host")

(define-error 'angelia-client-session-error
  "Angelia session error (missing id or unknown session)")

(cl-defstruct (angelia-client--conn
               (:constructor angelia-client--conn-create))
  "Bookkeeping for one live host connection.
The `sessions' slot is a hash from server-issued session id (string) to a
plist `(:on-event FN :on-end FN)' registered by `angelia-client-open-session'.
The notification dispatcher in `angelia-client-connect' uses it to route
`session/event' notifications back to the caller that opened them."
  host process jsonrpc stderr-buffer
  (sessions (make-hash-table :test #'equal)))

(defvar angelia-client--connections (make-hash-table :test #'equal)
  "Map HOST (string) -> live `angelia-client--conn'.")

;; ---------------------------------------------------------------------------
;; Helpers.

(defun angelia-client--ssh-args (host remote-path)
  "Build the argv list that launches the remote server on HOST.
Uses `-Q' so that site-init / user customizations on the remote can never
print to stdout and corrupt the protocol stream."
  (list "ssh" host "emacs" "-Q" "--batch" "-l" remote-path
        "-f" "angelia-server-main"))

(defun angelia-client--stderr-buffer-name (host)
  "Return the buffer name that captures the remote process's stderr for HOST."
  (format "*angelia-stderr<%s>*" host))

(defun angelia-client--make-stderr-buffer (host)
  "Return (creating as needed) the stderr capture buffer for HOST."
  (let ((buf (get-buffer-create (angelia-client--stderr-buffer-name host))))
    (with-current-buffer buf
      (unless (eq major-mode 'fundamental-mode) (fundamental-mode))
      (setq buffer-read-only nil
            truncate-lines t))
    buf))

(defun angelia-client--existing-live-connection (host)
  "Return the connection registered for HOST iff its process is still alive."
  (when-let ((conn (gethash host angelia-client--connections)))
    (let ((proc (angelia-client--conn-process conn)))
      (when (and proc (process-live-p proc))
        conn))))

(defun angelia-client--read-host (&optional prompt)
  "Read a host string from the minibuffer, completing against active hosts."
  (let ((hosts (hash-table-keys angelia-client--connections)))
    (if hosts
        (completing-read (or prompt "Host: ") hosts nil nil nil nil (car hosts))
      (read-string (or prompt "Host: ")))))

(defun angelia-client--on-shutdown (host)
  "Run when the jsonrpc connection for HOST shuts down.  Drops it from the map."
  (angelia-client--log "shutdown callback fired for host=%s" host)
  (when (fboundp 'angelia-client-lsp--cleanup-host)
    (angelia-client-lsp--cleanup-host host))
  (when-let ((conn (gethash host angelia-client--connections)))
    (when (process-live-p (angelia-client--conn-process conn))
      (delete-process (angelia-client--conn-process conn))))
  (remhash host angelia-client--connections))

;; ---------------------------------------------------------------------------
;; Connect / disconnect.

(defun angelia-client-connect (host)
  "Connect to HOST (an `ssh' destination string).  Returns the connection.
Idempotent: a live connection for HOST is returned unchanged.  Deploys the
server source on demand, spawns `ssh host emacs --batch -l <path> -f
angelia-server-main', wraps the pipe in `jsonrpc-process-connection', and
verifies the server's SHA1 matches the bundled client copy.  Signals
`angelia-client-version-mismatch' on hash divergence; signals other errors
on transport / handshake failure (cleaning up the dead process first)."
  (interactive (list (read-string "Host: " "localhost")))
  (or (angelia-client--existing-live-connection host)
      (let (proc jrpc conn stderr-buf remote-path cmd)
        (angelia-client--log "connect: starting host=%s" host)
        (setq remote-path (angelia-client-deploy host)
              stderr-buf (angelia-client--make-stderr-buffer host)
              cmd (angelia-client--ssh-args host remote-path))
        (angelia-client--log "connect: ssh cmd=%S" cmd)
        (condition-case err
            (progn
              (setq proc (make-process
                          :name (format "angelia-ssh<%s>" host)
                          :command cmd
                          :coding 'binary
                          :connection-type 'pipe
                          :noquery t
                          :stderr stderr-buf))
              (angelia-client--log "connect: pid=%s" (process-id proc))
              (setq jrpc (make-instance
                          'jsonrpc-process-connection
                          :name (format "angelia<%s>" host)
                          :process proc
                          :notification-dispatcher
                          (lambda (_c method params)
                            (angelia-client--dispatch-notification
                             host method params))
                          :request-dispatcher
                          (lambda (_c method _params)
                            (angelia-client--log
                             "unexpected server request: %s" method)
                            (signal 'jsonrpc-error
                                    `((jsonrpc-error-code . -32601)
                                      (jsonrpc-error-message
                                       . ,(format "Client does not handle %s" method)))))
                          :on-shutdown
                          (lambda (_c) (angelia-client--on-shutdown host))))
              (setq conn (angelia-client--conn-create
                          :host host
                          :process proc
                          :jsonrpc jrpc
                          :stderr-buffer stderr-buf))
              (puthash host conn angelia-client--connections)
              ;; Handshake.  jsonrpc.el decodes results as plists with
              ;; keyword keys (`:sha1', `:emacs_version', etc.).
              (let* ((t0 (current-time))
                     (resp (jsonrpc-request jrpc 'server/version nil :timeout 10))
                     (remote-sha (plist-get resp :sha1)))
                (angelia-client--log
                 "handshake: %.1f ms remote-sha=%s embedded-sha=%s"
                 (* 1000 (float-time (time-subtract (current-time) t0)))
                 remote-sha angelia-client--server-sha1)
                (unless (equal remote-sha angelia-client--server-sha1)
                  (signal 'angelia-client-version-mismatch
                          (list :host host
                                :expected angelia-client--server-sha1
                                :got remote-sha))))
              (angelia-client--log "connect: ok host=%s" host)
              conn)
          (error
           (angelia-client--log-error "connect" err)
           (when (and proc (process-live-p proc)) (delete-process proc))
           (remhash host angelia-client--connections)
           (signal (car err) (cdr err)))))))

(defun angelia-client-disconnect (host)
  "Tear down the live connection to HOST, if any."
  (interactive (list (angelia-client--read-host "Disconnect host: ")))
  (when-let ((conn (gethash host angelia-client--connections)))
    (angelia-client--log "disconnect: host=%s" host)
    (condition-case err
        (jsonrpc-shutdown (angelia-client--conn-jsonrpc conn))
      (error (angelia-client--log-error "disconnect/jsonrpc-shutdown" err)))
    (when (process-live-p (angelia-client--conn-process conn))
      (delete-process (angelia-client--conn-process conn)))
    (remhash host angelia-client--connections)))

(defun angelia-client-connection (host)
  "Return the live connection for HOST or signal `angelia-client-not-connected'."
  (or (angelia-client--existing-live-connection host)
      (signal 'angelia-client-not-connected (list :host host))))

;; ---------------------------------------------------------------------------
;; Sessions.
;;
;; A `session' is a server-issued opaque string returned by methods that open
;; a stream (chunked file ops, PTY, ...).  The client registers per-session
;; callbacks here; the notification dispatcher below routes incoming
;; `session/event' notifications to them.  The terminal `kind: "end"' event
;; tears the registration down on the client side.

(defun angelia-client--dispatch-notification (host method params)
  "Route incoming notification METHOD/PARAMS for HOST.
Currently only `session/event' is meaningful; everything else is logged."
  (let ((method-str (if (symbolp method) (symbol-name method) method)))
    (angelia-client--log "notification[%s]: %s %s" host method-str
                         (angelia-client--truncate (format "%S" params) 300))
    (cond
     ((equal method-str "session/event")
      (angelia-client--handle-session-event host params))
     (t
      ;; Already logged above; nothing else to do.
      nil))))

(defun angelia-client--handle-session-event (host params)
  "Look up the callback for PARAMS->session on HOST and dispatch it."
  (let* ((conn (gethash host angelia-client--connections))
         (sessions (and conn (angelia-client--conn-sessions conn)))
         (session (plist-get params :session))
         (kind (plist-get params :kind))
         (entry (and sessions session (gethash session sessions))))
    (cond
     ((null entry)
      (angelia-client--log
       "session/event for unknown session=%s kind=%s (dropped)" session kind))
     ((equal kind "end")
      (let ((on-end (plist-get entry :on-end)))
        (remhash session sessions)
        (when on-end
          (condition-case err (funcall on-end params)
            (error
             (angelia-client--log-error
              (format "session=%s on-end" session) err))))))
     (t
      (let ((on-event (plist-get entry :on-event)))
        (when on-event
          (condition-case err (funcall on-event kind params)
            (error
             (angelia-client--log-error
              (format "session=%s on-event kind=%s" session kind) err)))))))))

(cl-defun angelia-client-open-session (host method params on-event
                                            &key on-end timeout)
  "Call METHOD on HOST with PARAMS and treat its result.session as a session id.
Register ON-EVENT (called as (KIND PARAMS-PLIST) for each non-terminal
event) and the optional ON-END (called once with the terminal PARAMS-PLIST).
Returns the session id (a string).  Signals `angelia-client-session-error'
if the server's response lacks a session id."
  (let* ((conn (angelia-client-connection host))
         (result (jsonrpc-request (angelia-client--conn-jsonrpc conn)
                                  method params
                                  :timeout (or timeout 30)))
         (session (plist-get result :session)))
    (unless (stringp session)
      (signal 'angelia-client-session-error
              (list :host host :method method :result result)))
    (puthash session
             (list :on-event on-event :on-end on-end)
             (angelia-client--conn-sessions conn))
    (angelia-client--log "session opened: host=%s session=%s method=%s"
                         host session method)
    session))

(defun angelia-client-send-to-session (host session method params &optional timeout)
  "Call METHOD on HOST with a fresh hash of PARAMS keys plus `session' = SESSION."
  (let ((p (make-hash-table :test #'equal)))
    (when (hash-table-p params)
      (maphash (lambda (k v) (puthash k v p)) params))
    (puthash "session" session p)
    (angelia-client-call host method p timeout)))

(defun angelia-client-close-session (host session)
  "Request the server end SESSION, then drop the local callback unconditionally."
  (when-let ((conn (gethash host angelia-client--connections)))
    (remhash session (angelia-client--conn-sessions conn)))
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" session p)
    (condition-case err
        (angelia-client-call host 'session/close p 5)
      (error
       (angelia-client--log-error
        (format "close-session host=%s session=%s" host session) err)))))

;; ---------------------------------------------------------------------------
;; RPC surface.

(defun angelia-client-call (host method &optional params timeout)
  "Synchronously call METHOD on HOST with PARAMS.  Returns the result.
TIMEOUT is seconds (default 30)."
  (let* ((conn (angelia-client-connection host))
         (id-buf (angelia-client--truncate (format "%S" params) 200))
         (t0 (current-time)))
    (angelia-client--log "call %s on %s params=%s" method host id-buf)
    (let* ((resp (jsonrpc-request (angelia-client--conn-jsonrpc conn)
                                  method params
                                  :timeout (or timeout 30)))
           (elapsed (* 1000 (float-time (time-subtract (current-time) t0)))))
      (angelia-client--log "call %s on %s ok %.1f ms result=%s"
                           method host elapsed
                           (angelia-client--truncate (format "%S" resp) 200))
      resp)))

(defun angelia-client-async (host method params success-fn &optional error-fn)
  "Asynchronously call METHOD on HOST with PARAMS.
SUCCESS-FN is called with the result; optional ERROR-FN with the error plist."
  (let ((conn (angelia-client-connection host)))
    (angelia-client--log "async-call %s on %s" method host)
    (jsonrpc-async-request (angelia-client--conn-jsonrpc conn)
                           method params
                           :success-fn success-fn
                           :error-fn error-fn)))

;; ---------------------------------------------------------------------------
;; Interactive commands.

(defun angelia-client-send-ping (host)
  "Round-trip a `server/ping' to HOST and report milliseconds."
  (interactive (list (angelia-client--read-host "Ping host: ")))
  (let* ((t0 (current-time))
         (resp (angelia-client-call host 'server/ping nil))
         (elapsed (* 1000 (float-time (time-subtract (current-time) t0)))))
    (message "angelia-ping[%s] %s in %.1f ms"
             host
             (if (eq (plist-get resp :pong) t) "ok" "FAIL")
             elapsed)))

(defun angelia-client-server-info (host)
  "Display the server's version/uptime/PID for HOST."
  (interactive (list (angelia-client--read-host "Info for host: ")))
  (let ((info (angelia-client-call host 'server/info nil)))
    (message "angelia@%s emacs=%s pid=%s uptime=%dms sha1=%s host=%s"
             host
             (plist-get info :emacs_version)
             (plist-get info :pid)
             (plist-get info :uptime_ms)
             (substring (plist-get info :sha1) 0 8)
             (plist-get info :hostname))))

(defun angelia-client-show-debug-log ()
  "Switch to the `*angelia-client-debug*' buffer."
  (interactive)
  (switch-to-buffer (angelia-client--debug-buffer)))

(defun angelia-client-clear-debug-log ()
  "Clear the `*angelia-client-debug*' buffer."
  (interactive)
  (with-current-buffer (angelia-client--debug-buffer)
    (let ((inhibit-read-only t)) (erase-buffer))))

(provide 'angelia-client)
;;; angelia-client.el ends here
