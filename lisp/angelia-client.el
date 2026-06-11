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
`session/event' notifications back to the caller that opened them.

`pending-events' buffers events that arrive for a session BEFORE its
callbacks are registered: jsonrpc.el's process filter dispatches every
complete message in one pass, so a response and its first notifications can
both be processed before the synchronous requester regains control to
register.  `angelia-client-register-session' replays the queue.

`closed-sessions' holds tombstones (session id -> close time) for sessions
this side closed on purpose, so their late events are dropped instead of
queued."
  host process jsonrpc stderr-buffer
  (sessions (make-hash-table :test #'equal))
  (pending-events (make-hash-table :test #'equal))
  (closed-sessions (make-hash-table :test #'equal)))

(defvar angelia-client--connections (make-hash-table :test #'equal)
  "Map HOST (string) -> live `angelia-client--conn'.")

(defcustom angelia-client-auto-reconnect t
  "When non-nil, transparently re-establish a connection that dropped unexpectedly.
An explicit `angelia-client-disconnect' is never auto-reconnected.  Two paths use
this: the next `angelia-client-call' reconnects on demand, and the shutdown
sentinel schedules a background reconnect with backoff."
  :type 'boolean
  :group 'angelia)

(defcustom angelia-client-reconnect-max-attempts 5
  "Maximum background reconnect attempts after an unexpected drop (0 disables).
Each attempt waits `angelia-client-reconnect-base-delay' doubled per try."
  :type 'integer
  :group 'angelia)

(defcustom angelia-client-reconnect-base-delay 1.0
  "Seconds before the first background reconnect attempt; doubles each retry."
  :type 'number
  :group 'angelia)

(defvar angelia-client-after-connect-functions nil
  "Abnormal hook run with one argument HOST after a successful (re)connect.
`angelia-client-files' uses it to re-register live file-notify watches once a
dropped connection comes back.  Runs on the initial connect too (a no-op when
the host has no prior state).")

(defvar angelia-client--disconnecting (make-hash-table :test #'equal)
  "Set of hosts (string -> t) currently being torn down on purpose.
The shutdown sentinel consults it so an explicit `angelia-client-disconnect'
is not mistaken for an unexpected drop and auto-reconnected.")

(defvar angelia-client--reconnecting (make-hash-table :test #'equal)
  "Set of hosts (string -> t) with a background reconnect already in flight,
so overlapping shutdown events don't stack multiple reconnect loops.")

;; ---------------------------------------------------------------------------
;; Helpers.

(defun angelia-client--ssh-args (host remote-path)
  "Build the argv list that launches the remote server on HOST.
Creates a named FIFO on the remote, copies SSH stdin into it via a
background `cat', then starts emacs reading from the FIFO via
ANGELIA_STDIN_FIFO.  This avoids the Linux-only /proc/PID/fd/0 trick
and works on macOS (and any POSIX host with mktemp + mkfifo).
The whole script runs under the remote login environment regardless of the
default login shell (csh/tcsh included); see `angelia-client--login-wrap'."
  (let* ((emacs-cmd (concat (if angelia-client-debug "ANGELIA_DEBUG=1 " "")
                            (if (> angelia-client-simulated-delay-ms 0)
                                (format "ANGELIA_DELAY_MS=%d "
                                        angelia-client-simulated-delay-ms)
                              "")
                            "ANGELIA_STDIN_FIFO=\"$f\" "
                            "emacs -Q --batch -l "
                            remote-path   ; tilde path: must remain unquoted for bash expansion
                            " -f angelia-server-main"))
         ;; exec 3<&0 saves SSH stdin before bash nullifies it for background jobs.
         ;; <&3 in the cat invocation is an explicit redirect that overrides the
         ;; implicit /dev/null redirection bash applies to async commands.
         (script    (concat "exec 3<&0; f=$(mktemp -u) && mkfifo -m 0600 \"$f\" && "
                            "{ cat <&3 > \"$f\" 3<&- & } && exec 3<&- && "
                            emacs-cmd "; rm -f \"$f\"")))
    (append
     (list angelia-client-ssh-program)
     ;; Keepalive so a dropped link is detected promptly instead of hanging on
     ;; a half-open socket; this is what lets auto-reconnect kick in.
     (when (> angelia-client-keepalive-interval 0)
       (list "-o" (format "ServerAliveInterval=%d" angelia-client-keepalive-interval)
             "-o" (format "ServerAliveCountMax=%d" angelia-client-keepalive-count)))
     (list host (angelia-client--login-wrap host script)))))

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

(defun angelia-client--on-shutdown (host &optional proc)
  "Run when the jsonrpc connection for HOST shuts down.  Drops it from the map.
PROC, when given, is the process this shutdown belongs to; if the host is now
registered under a DIFFERENT process (a newer connection already took its
place), this is a stale shutdown and we do nothing -- otherwise it would tear
down the live replacement.  If the drop was unexpected (not an explicit
`angelia-client-disconnect') and `angelia-client-auto-reconnect' is on, schedule
a background reconnect."
  (let ((conn (gethash host angelia-client--connections)))
    (if (and proc conn (not (eq (angelia-client--conn-process conn) proc)))
        (angelia-client--log "stale shutdown for host=%s (superseded); ignoring" host)
      (angelia-client--log "shutdown callback fired for host=%s" host)
      (when (fboundp 'angelia-client-lsp--cleanup-host)
        (angelia-client-lsp--cleanup-host host))
      (when (and conn (process-live-p (angelia-client--conn-process conn)))
        (delete-process (angelia-client--conn-process conn)))
      (remhash host angelia-client--connections)
      (when (and angelia-client-auto-reconnect
                 (> angelia-client-reconnect-max-attempts 0)
                 (not (gethash host angelia-client--disconnecting))
                 (not (gethash host angelia-client--reconnecting)))
        (puthash host t angelia-client--reconnecting)
        (angelia-client--log "scheduling auto-reconnect for host=%s" host)
        (run-at-time angelia-client-reconnect-base-delay nil
                     #'angelia-client--attempt-reconnect host 1)))))

(defun angelia-client--attempt-reconnect (host attempt)
  "Try to reconnect to HOST (ATTEMPT-th try); retry with backoff or give up.
On success runs `angelia-client-after-connect-functions' (already triggered by
`angelia-client-connect') and clears the in-flight flag."
  ;; A live connection may have been re-established meanwhile (e.g. a call-time
  ;; reconnect); if so we are done.
  (if (angelia-client--existing-live-connection host)
      (remhash host angelia-client--reconnecting)
    (condition-case err
        (progn
          (angelia-client--log "auto-reconnect attempt %d for host=%s" attempt host)
          (angelia-client-connect host)
          (remhash host angelia-client--reconnecting)
          (angelia-client--log "auto-reconnect ok for host=%s" host))
      (error
       (angelia-client--log-error
        (format "auto-reconnect attempt %d host=%s" attempt host) err)
       (if (< attempt angelia-client-reconnect-max-attempts)
           (run-at-time (* angelia-client-reconnect-base-delay (expt 2 attempt)) nil
                        #'angelia-client--attempt-reconnect host (1+ attempt))
         (remhash host angelia-client--reconnecting)
         (angelia-client--log "auto-reconnect gave up for host=%s" host))))))

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
                          (lambda (_c) (angelia-client--on-shutdown host proc))))
              (setq conn (angelia-client--conn-create
                          :host host
                          :process proc
                          :jsonrpc jrpc
                          :stderr-buffer stderr-buf))
              (puthash host conn angelia-client--connections)
              ;; Handshake.  jsonrpc.el decodes results as plists with
              ;; keyword keys (`:sha1', `:emacs_version', etc.).
              (let* ((resp (tempus-measure (format "client handshake server/version on %s" host)
                             (jsonrpc-request jrpc 'server/version nil :timeout 10)))
                     (remote-sha (plist-get resp :sha1)))
                (angelia-client--log
                 "handshake: remote-sha=%s embedded-sha=%s"
                 remote-sha angelia-client--server-sha1)
                (unless (equal remote-sha angelia-client--server-sha1)
                  (signal 'angelia-client-version-mismatch
                          (list :host host
                                :expected angelia-client--server-sha1
                                :got remote-sha))))
              (angelia-client--log "connect: ok host=%s" host)
              ;; Let dependents (e.g. file-notify watch re-registration) react to
              ;; a fresh connection.  Errors in a hook fn must not fail connect.
              (run-hook-with-args 'angelia-client-after-connect-functions host)
              conn)
          (error
           (angelia-client--log-error "connect" err)
           (when (and proc (process-live-p proc)) (delete-process proc))
           (remhash host angelia-client--connections)
           (signal (car err) (cdr err)))))))

(defun angelia-client-disconnect (host)
  "Tear down the live connection to HOST, if any.
Marks HOST as intentionally disconnecting so the shutdown sentinel does not
auto-reconnect it."
  (interactive (list (angelia-client--read-host "Disconnect host: ")))
  (when-let ((conn (gethash host angelia-client--connections)))
    (angelia-client--log "disconnect: host=%s" host)
    (puthash host t angelia-client--disconnecting)
    (unwind-protect
        (progn
          (condition-case err
              (jsonrpc-shutdown (angelia-client--conn-jsonrpc conn))
            (error (angelia-client--log-error "disconnect/jsonrpc-shutdown" err)))
          (when (process-live-p (angelia-client--conn-process conn))
            (delete-process (angelia-client--conn-process conn)))
          (remhash host angelia-client--connections))
      (remhash host angelia-client--disconnecting))))

(defun angelia-client-reconnect (host)
  "Disconnect from HOST (if connected) then establish a fresh connection.
Returns the new connection.  Useful when the SSH pipe has dropped or the
remote server is unresponsive."
  (interactive (list (angelia-client--read-host "Reconnect host: ")))
  (angelia-client-disconnect host)
  (angelia-client-connect host))

(defun angelia-client-restart-server (host)
  "Reconnect to HOST, forcing a re-upload of the server source first.
Unlike `angelia-client-reconnect', the SHA1 check is bypassed so the
embedded server.el is always re-deployed before the new connection starts.
Use this to pick up local changes to the server source without incrementing
the SHA1 manually, or to recover a host with a corrupted remote copy."
  (interactive (list (angelia-client--read-host "Restart server on host: ")))
  (angelia-client-disconnect host)
  (angelia-client-deploy host 'force)
  (angelia-client-connect host))

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

(defconst angelia-client--pending-events-ttl 30
  "Seconds queued events for a never-registered session are kept before pruning.")

(defconst angelia-client--pending-events-max 4096
  "Cap on queued events per unregistered session; overflow events are dropped.")

(defun angelia-client--prune-session-table (table)
  "Drop entries of TABLE (id -> (TIME . _)) older than the pending-events TTL.
Used for both the pending-event queues and the closed-session tombstones."
  (let (stale)
    (maphash (lambda (sid cell)
               (let ((ts (if (consp cell) (car cell) cell)))
                 (when (> (float-time (time-subtract (current-time) ts))
                          angelia-client--pending-events-ttl)
                   (push sid stale))))
             table)
    (dolist (sid stale) (remhash sid table))))

(defun angelia-client--queue-pending-event (conn session params)
  "Buffer PARAMS for SESSION on CONN until its callbacks are registered.
The race this covers: jsonrpc.el dispatches every complete message in one
process-filter pass, so the events following a method response can arrive
before the requester has registered the session id the response carries.
Dropping them loses chunks / exit events; queueing + replay on registration
makes the stream lossless.  Queues are pruned after
`angelia-client--pending-events-ttl' so an opener that never registers
\(or events for a genuinely unknown session) cannot leak memory."
  (let ((pending (angelia-client--conn-pending-events conn)))
    (angelia-client--prune-session-table pending)
    (let ((cell (or (gethash session pending)
                    (puthash session (cons (current-time) nil) pending))))
      (if (>= (length (cdr cell)) angelia-client--pending-events-max)
          (angelia-client--log
           "session/event overflow for unregistered session=%s (dropped)" session)
        (setcdr cell (cons params (cdr cell)))))))

(defun angelia-client--handle-session-event (host params)
  "Look up the callback for PARAMS->session on HOST and dispatch it.
Events for a session with no registered callback are queued for replay
\(see `angelia-client--queue-pending-event') unless the session was closed
on purpose (tombstoned), in which case they are dropped."
  (let* ((conn (gethash host angelia-client--connections))
         (sessions (and conn (angelia-client--conn-sessions conn)))
         (session (plist-get params :session))
         (kind (plist-get params :kind))
         (entry (and sessions session (gethash session sessions))))
    (cond
     ((null entry)
      (cond
       ((or (null conn) (not (stringp session)))
        (angelia-client--log
         "session/event for unknown session=%s kind=%s (dropped)" session kind))
       ((gethash session (angelia-client--conn-closed-sessions conn))
        (angelia-client--log
         "session/event for closed session=%s kind=%s (dropped)" session kind))
       (t
        (angelia-client--log
         "session/event for unregistered session=%s kind=%s (queued)" session kind)
        (angelia-client--queue-pending-event conn session params))))
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

(defun angelia-client-register-session (conn session on-event on-end)
  "Register ON-EVENT / ON-END for SESSION on CONN; replay queued events.
This is THE registration point for session callbacks -- every opener
\(`angelia-client-open-session', the proc/exec wrappers, ...) must go
through it, because it also drains the pending-event queue: events that
were dispatched before the opener regained control (response + first
notifications processed in one jsonrpc filter pass) are replayed here in
arrival order, so the stream is lossless.  Returns SESSION."
  (puthash session
           (list :on-event on-event :on-end on-end)
           (angelia-client--conn-sessions conn))
  (let* ((pending (angelia-client--conn-pending-events conn))
         (cell (gethash session pending)))
    (when cell
      (remhash session pending)
      (let ((events (nreverse (cdr cell))))
        (angelia-client--log "session=%s replaying %d queued event(s)"
                             session (length events))
        (dolist (params events)
          (angelia-client--handle-session-event
           (angelia-client--conn-host conn) params)))))
  session)

(defun angelia-client-deregister-session (conn session)
  "Drop SESSION's callbacks from CONN and tombstone it.
The tombstone makes late events for the deliberately-closed session drop
instead of queueing for replay.  Tombstones are pruned on the same TTL as
the pending queues."
  (remhash session (angelia-client--conn-sessions conn))
  (remhash session (angelia-client--conn-pending-events conn))
  (let ((closed (angelia-client--conn-closed-sessions conn)))
    (angelia-client--prune-session-table closed)
    (puthash session (current-time) closed)))

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
    (angelia-client-register-session conn session on-event on-end)
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
    (angelia-client-deregister-session conn session))
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" session p)
    (condition-case err
        (angelia-client-call host 'session/close p 5)
      (error
       (angelia-client--log-error
        (format "close-session host=%s session=%s" host session) err)))))

;; ---------------------------------------------------------------------------
;; RPC surface.

(defun angelia-client--call-once (host method params timeout)
  "Issue one METHOD request to HOST with PARAMS; return the result."
  (let* ((conn (angelia-client-connection host))
         (id-buf (angelia-client--truncate (format "%S" params) 200)))
    (angelia-client--log "call %s on %s params=%s" method host id-buf)
    (let ((resp (tempus-measure (format "client call %s on %s" method host)
                  (jsonrpc-request (angelia-client--conn-jsonrpc conn)
                                   method params
                                   :timeout (or timeout 30)))))
      (angelia-client--log "call %s on %s ok result=%s"
                           method host
                           (angelia-client--truncate (format "%S" resp) 200))
      resp)))

(defconst angelia-client--idempotent-methods
  '(server/ping server/version server/info server/lsp-programs
    file/read file/exists file/directory-p file/attributes
    file/list-dir file/list-dir-attrs file/completions file/fs-info
    file/symlink-target file/writable-p file/executable-p file/locked-p
    file/search file/watch proc/list-persisted)
  "Methods safe to re-issue after a MID-FLIGHT connection drop.
When the link dies while a request is pending, the request can have
EXECUTED on the server even though its response never arrived, so only
side-effect-free reads -- or session openers, whose pre-drop session died
with the old server anyway -- may be silently replayed.  Mutations
\(`file/delete', `file/rename', `file/write-finish', ...) would run twice;
their error is re-signalled and the caller decides.

This gate does NOT apply when the connection was already known-dead before
the call (`angelia-client-not-connected'): such a request was never sent,
so replaying any method after the reconnect is safe.")

(defun angelia-client-call (host method &optional params timeout)
  "Synchronously call METHOD on HOST with PARAMS.  Returns the result.
TIMEOUT is seconds (default 30).  If the connection has dropped and
`angelia-client-auto-reconnect' is on, reconnect and retry transparently --
with one safety gate: a request that was already IN FLIGHT when the link
died (`jsonrpc-error' & friends) may have executed on the server, so it is
replayed only when METHOD is in `angelia-client--idempotent-methods';
replaying a mutation could perform it twice.  A request that was never sent
\(the connection was already known-dead: `angelia-client-not-connected') is
safe to issue after the reconnect whatever the method.  Genuine method
errors (file not found, etc.) on a live connection are re-signalled
untouched."
  (condition-case err
      (angelia-client--call-once host method params timeout)
    ((angelia-client-not-connected jsonrpc-error error)
     (if (and angelia-client-auto-reconnect
              (not (angelia-client--existing-live-connection host)))
         (progn
           (angelia-client--log
            "call %s on %s: connection down (%s), reconnecting"
            method host (error-message-string err))
           (ignore-errors (angelia-client-disconnect host))
           (angelia-client-connect host)
           (if (or (eq (car err) 'angelia-client-not-connected)
                   (memq (if (stringp method) (intern method) method)
                         angelia-client--idempotent-methods))
               (angelia-client--call-once host method params timeout)
             (angelia-client--log
              "call %s on %s: in-flight at drop and not idempotent, not replayed"
              method host)
             (signal (car err) (cdr err))))
       (signal (car err) (cdr err))))))

(defun angelia-client-async (host method params success-fn &optional error-fn)
  "Asynchronously call METHOD on HOST with PARAMS.
SUCCESS-FN is called with the result; optional ERROR-FN with the error plist.
The round-trip is timed via Tempus (the call returns immediately, so timing is
logged from the wrapping callbacks rather than a body wrap)."
  (let ((conn (angelia-client-connection host))
        (t0 (current-time))
        (label (format "client async %s on %s" method host)))
    (angelia-client--log "async-call %s on %s" method host)
    (jsonrpc-async-request
     (angelia-client--conn-jsonrpc conn)
     method params
     :success-fn (lambda (result)
                   (tempus-log-since label t0)
                   (when success-fn (funcall success-fn result)))
     :error-fn (lambda (err)
                 (tempus-log-since label t0)
                 (when error-fn (funcall error-fn err))))))

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

;; Load the file-name-handler after provide to avoid a circular dependency:
;; angelia-client-files requires angelia-client, so angelia-client must be in
;; `features' before the sub-file is loaded.
(require 'angelia-client-files)

;;; angelia-client.el ends here
