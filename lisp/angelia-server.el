;;; -*- lexical-binding: t; -*-
;;; angelia-server.el --- Headless remote half of Angelia Emacs Remote-SSH

;; This file is deployed by `angelia-client-deploy' to ~/.cache/angelia/server.el
;; on the remote host and executed via:
;;
;;   emacs --batch -l server.el -f angelia-server-main
;;
;; Wire protocol: JSON-RPC 2.0 with HTTP-style `Content-Length' framing over
;; stdin/stdout.  Stdout is sacred -- only protocol bytes ever go there.  Every
;; log line, warning, error, and backtrace is written to stderr via
;; `external-debugging-output'.  See CLAUDE.md in the repo root for the
;; non-negotiable invariants this file must respect.

(require 'cl-lib)
(require 'subr-x)
;; `filenotify' backs `file/watch': the server watches real paths with the
;; host's native notification backend (inotify on Linux) and streams changes.
(require 'filenotify)

;; ---------------------------------------------------------------------------
;; Forward declarations.  Hoisted here so byte-compilation accepts closures
;; that mutate these from inside `angelia-server--make-filter' etc.

(defvar angelia-server--methods (make-hash-table :test #'equal)
  "Map JSON-RPC method name (string) -> handler (function of CONN PARAMS).")

(defvar angelia-server--inbuf (unibyte-string)
  "Accumulated unibyte stdin bytes not yet consumed into a complete frame.")

(defvar angelia-server--quit-flag nil
  "When non-nil, the main event loop should exit cleanly.")

(defvar angelia-server--source-sha1 nil
  "SHA1 of this server file's bytes, populated at load time.")

(defvar angelia-server--start-time nil
  "Wall-clock time at which the server entered `angelia-server-main'.")

(defvar angelia-server--response-delay-ms 0
  "Artificial per-response delay in milliseconds, for simulating a slow link.
Set from the ANGELIA_DELAY_MS env var in `angelia-server-main'; 0 disables.
Purely a testing aid -- it lets a localhost connection mimic real latency so
Tempus round-trip numbers look like a remote host.")

(defvar angelia-server--sessions (make-hash-table :test #'equal)
  "Server-global session registry: session-id (string) -> state plist.
Long-lived streaming methods (chunked file I/O, PTY, ...) allocate a row
here and key all subsequent events on the returned id.  Per-feature state
lives in the plist (e.g. (:kind read-stream :offset N :total M)).")

(defvar angelia-server--proc-backends nil
  "Alist of (NAME . `angelia-server--proc-backend' struct) for installed
persistence backends.  Populated lazily by
`angelia-server--init-proc-backends'; hoisted here so functions earlier
in the file (e.g. `server/info') can reference it without a byte-compile
warning.")

(define-error 'angelia-server-protocol-error "Angelia server protocol error")

;; ---------------------------------------------------------------------------
;; Debug logging -- stderr only, never stdout.

(defsubst angelia-server--ts ()
  "Return a millisecond-resolution timestamp for log lines."
  (format-time-string "%H:%M:%S.%3N"))

(defun angelia-server--log (fmt &rest args)
  "Write a debug line to stderr.  Never call `message' or `princ' to stdout."
  (princ (format "[SERVER %s] %s\n" (angelia-server--ts)
                 (apply #'format fmt args))
         #'external-debugging-output))

(defun angelia-server--log-error (err)
  "Log error ERR (a `condition-case' object) and a backtrace to stderr."
  (angelia-server--log "ERROR %S: %s" (car err) (error-message-string err))
  (let ((bt (with-output-to-string
              (let ((standard-output (current-buffer)))
                (backtrace)))))
    (when (and bt (> (length bt) 0))
      (angelia-server--log "Backtrace:\n%s" bt))))

(defun angelia-server--truncate (s n)
  "Return S clipped to at most N characters, with an ellipsis when clipped."
  (if (<= (length s) n) s (concat (substring s 0 n) "…")))

;; ---------------------------------------------------------------------------
;; Tempus -- debug-gated timing.  Inlined copy of lisp/tempus.el: the server is
;; deployed as a single file (one SHA1 handshake), so we cannot `require' the
;; standalone package on the remote.  Keep this in sync with lisp/tempus.el.
;; `tempus-debug' is wired from ANGELIA_DEBUG and `tempus-log-function' to
;; `angelia-server--log' in `angelia-server-main'.

(defvar tempus-debug nil
  "When non-nil, `tempus-measure' logs elapsed timings; otherwise zero overhead.")

(defvar tempus-log-function #'ignore
  "Function emitting one timing line.
Called as (apply tempus-log-function FMT ARGS).")

(defun tempus-log-since (label start)
  "Log the elapsed ms since START (a time value) under LABEL, if `tempus-debug'."
  (when tempus-debug
    (apply tempus-log-function
           "tempus %s | %.1f ms"
           (list label
                 (* 1000 (float-time (time-subtract (current-time) start)))))))

(defmacro tempus-measure (label &rest body)
  "Evaluate BODY once and return its value.
When `tempus-debug' is non-nil, log the time BODY took (ms) via
`tempus-log-function', keyed by LABEL.  Recorded even on non-local exit."
  (declare (indent 1) (debug (form body)))
  (let ((t0 (gensym "t0-"))
        (lbl (gensym "lbl-")))
    `(let ((,t0 (current-time))
           (,lbl ,label))
       (unwind-protect
           (progn ,@body)
         (tempus-log-since ,lbl ,t0)))))

;; ---------------------------------------------------------------------------
;; Connection context + method registry.
;;
;; CONN is passed to every handler so future handlers can send unsolicited
;; notifications or stream output.  Today the field is unused; the shape is
;; what makes streaming additions cost-free later.

(cl-defstruct (angelia-server--conn
               (:constructor angelia-server--conn-create))
  "Per-server context handed to each method handler."
  proc)

(defun angelia-server-register-method (name handler)
  "Register HANDLER as the implementation of method NAME.
HANDLER is called as (funcall HANDLER CONN PARAMS) and must either return a
JSON-serializable value (sent as the JSON-RPC result) or signal `error' (sent
as a JSON-RPC -32603 error)."
  (puthash name handler angelia-server--methods))

(defun angelia-server-unregister-method (name)
  "Remove the handler registered under method NAME, if any."
  (remhash name angelia-server--methods))

;; ---------------------------------------------------------------------------
;; Content-Length framing.

(defun angelia-server--frame-bytes (buf)
  "Try to peel one frame off the front of BUF (a unibyte string).
Return (BODY-BYTES . REMAINING-BUF) when a complete frame is available, nil
otherwise.  Signal `angelia-server-protocol-error' if BUF contains a header
section that lacks a Content-Length line."
  (let ((sep (string-match "\r\n\r\n" buf)))
    (when sep
      (let* ((headers (substring buf 0 sep))
             (body-start (+ sep 4))
             (len (when (string-match "Content-Length:[ \t]*\\([0-9]+\\)" headers)
                    (string-to-number (match-string 1 headers)))))
        (unless len
          (signal 'angelia-server-protocol-error
                  (list "Missing Content-Length header" headers)))
        (when (>= (- (length buf) body-start) len)
          (cons (substring buf body-start (+ body-start len))
                (substring buf (+ body-start len))))))))

(defun angelia-server--write-frame (payload)
  "Serialize PAYLOAD (hash-table or alist) and write it framed to stdout.
PAYLOAD must be a value `json-serialize' accepts.  After writing the bytes,
`send-string-to-terminal' calls `fflush(stdout)' so the local client never
hangs waiting on a buffered response."
  (let* ((body (json-serialize payload))
         (body-bytes (encode-coding-string body 'utf-8 t))
         (header-bytes (encode-coding-string
                        (format "Content-Length: %d\r\n\r\n" (length body-bytes))
                        'utf-8 t))
         (frame (concat header-bytes body-bytes)))
    (angelia-server--log "stdout: %s" (angelia-server--truncate body 500))
    (send-string-to-terminal frame)
    (angelia-server--log "flushed %d bytes" (length frame))))

;; ---------------------------------------------------------------------------
;; JSON-RPC envelope helpers.

(defun angelia-server--has-id-p (id)
  "Return non-nil when ID denotes a real JSON-RPC request id (not a notification).
The wire absence of `id' surfaces here as nil; an explicit JSON null becomes
the `:null' keyword.  Both mean \"no response expected\"."
  (and id (not (eq id :null))))

(defun angelia-server--make-error (id code message &optional data)
  "Build a JSON-RPC error response hash-table for ID with CODE/MESSAGE/DATA."
  (let ((err (make-hash-table :test #'equal))
        (resp (make-hash-table :test #'equal)))
    (puthash "code" code err)
    (puthash "message" message err)
    (when data (puthash "data" data err))
    (puthash "jsonrpc" "2.0" resp)
    (puthash "id" (or id :null) resp)
    (puthash "error" err resp)
    resp))

(defun angelia-server--make-result (id result)
  "Build a JSON-RPC success response hash-table for ID carrying RESULT."
  (let ((resp (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" resp)
    (puthash "id" id resp)
    (puthash "result" result resp)
    resp))

;; ---------------------------------------------------------------------------
;; Sessions.
;;
;; A session is a server-generated opaque string returned by methods that open
;; a stream (chunked file ops, PTYs, ...).  The client registers a callback
;; against the id; the server pushes `session/event' notifications carrying
;; `{session, kind, ...}'.  A terminal event with `kind: "end"' tears the
;; session down on both sides.

(defun angelia-server--make-session-id ()
  "Return a fresh, never-before-seen session id of the form `s-<16-hex>'."
  (let (id)
    (while (or (null id) (gethash id angelia-server--sessions))
      (setq id (format "s-%016x" (random (expt 2 64)))))
    id))

(defun angelia-server--send-notification (_conn method params)
  "Write a JSON-RPC notification (no `id') with METHOD and PARAMS to stdout.
PARAMS may be a hash-table, a JSON-serialisable list, or nil."
  (let ((env (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" env)
    (puthash "method" method env)
    (when params (puthash "params" params env))
    (angelia-server--write-frame env)))

(defun angelia-server--send-session-event (conn session kind &optional payload)
  "Push a `session/event' notification for SESSION carrying KIND.
PAYLOAD, when non-nil, is a hash-table of additional fields to merge into
the notification params (e.g. {data: STR-base64} for chunk events)."
  (let ((params (make-hash-table :test #'equal)))
    (puthash "session" session params)
    (puthash "kind" kind params)
    (when (hash-table-p payload)
      (maphash (lambda (k v) (puthash k v params)) payload))
    (angelia-server--send-notification conn "session/event" params)))

(defun angelia-server--cleanup-session (state)
  "Run kind-specific resource cleanup for STATE.
Called from `angelia-server--end-session' before the row is removed."
  (pcase (plist-get state :kind)
    ('file-write
     (let ((tmp (plist-get state :tmp)))
       (when (and tmp (file-exists-p tmp))
         (ignore-errors (delete-file tmp)))))
    ('proc
     (let ((proc (plist-get state :process)))
       (when (and proc (process-live-p proc))
         (ignore-errors (delete-process proc)))))
    ('proc-exec
     (let ((proc (plist-get state :process))
           (stderr-pipe (plist-get state :stderr-pipe)))
       (when (and proc (process-live-p proc))
         (ignore-errors (delete-process proc)))
       (when (and stderr-pipe (process-live-p stderr-pipe))
         (ignore-errors (delete-process stderr-pipe)))))
    ('file-watch
     (let ((desc (plist-get state :descriptor)))
       (when desc (ignore-errors (file-notify-rm-watch desc)))))))

(defun angelia-server--end-session (conn session)
  "Emit a terminal `kind: \"end\"' event for SESSION and drop its state row.
Runs per-kind resource cleanup (see `angelia-server--cleanup-session') first
so abandoned streams release temp files / processes / etc. before the row
disappears.  Safe to call on already-dead sessions; in that case it is a
no-op."
  (when-let ((state (gethash session angelia-server--sessions)))
    (angelia-server--cleanup-session state)
    (angelia-server--send-session-event conn session "end" nil)
    (remhash session angelia-server--sessions)))

(defun angelia-server--register-session (session state)
  "Add a row for SESSION in the registry with STATE (a plist)."
  (puthash session state angelia-server--sessions))

;; ---------------------------------------------------------------------------
;; Dispatch.

(defun angelia-server--parse-delay-ms (s)
  "Parse env string S into a non-negative response delay in ms.
Returns 0 when S is nil, blank, or not a positive number."
  (let ((n (and s (string-to-number s))))
    (if (and n (> n 0)) n 0)))

(defun angelia-server--simulate-delay ()
  "Block for `angelia-server--response-delay-ms' ms to mimic connection latency.
No-op when the delay is 0.  Called once per request in
`angelia-server--dispatch', so every response is held back as on a slow link."
  (when (> angelia-server--response-delay-ms 0)
    (sleep-for (/ angelia-server--response-delay-ms 1000.0))))

(defun angelia-server--dispatch (conn frame)
  "Dispatch FRAME (a parsed JSON-RPC envelope) through the method registry.
CONN is the connection context, passed verbatim to every handler."
  (let* ((id (gethash "id" frame))
         (method (gethash "method" frame))
         (params (gethash "params" frame))
         (handler (and method (gethash method angelia-server--methods))))
    (angelia-server--log "dispatch: method=%S id=%S handler=%S"
                         method id (and handler t))
    ;; Simulate connection latency before any response is produced.  Sits
    ;; outside the handler's `tempus-measure' below, so server-side handler
    ;; timing stays honest while the client's round-trip reflects the delay.
    (angelia-server--simulate-delay)
    (cond
     ((not (stringp method))
      (when (angelia-server--has-id-p id)
        (angelia-server--write-frame
         (angelia-server--make-error id -32600
                                     "Invalid Request: missing or non-string method"))))
     ((null handler)
      (when (angelia-server--has-id-p id)
        (angelia-server--write-frame
         (angelia-server--make-error id -32601
                                     (format "Method not found: %s" method)))))
     (t
      (condition-case err
          (let ((result (tempus-measure method
                          (funcall handler conn params))))
            (when (angelia-server--has-id-p id)
              (angelia-server--write-frame
               (angelia-server--make-result id result))))
        (error
         (angelia-server--log-error err)
         (when (angelia-server--has-id-p id)
           (angelia-server--write-frame
            (angelia-server--make-error id -32603
                                        (error-message-string err))))))))))

;; ---------------------------------------------------------------------------
;; Process filter (binds stdin bytes -> dispatch).

(defun angelia-server--make-filter (conn)
  "Return a process filter that consumes stdin bytes for CONN."
  (lambda (_proc output)
    (angelia-server--log "stdin: %s" (angelia-server--truncate output 500))
    (setq angelia-server--inbuf (concat angelia-server--inbuf output))
    (let (extracted)
      (condition-case err
          (while (setq extracted
                       (angelia-server--frame-bytes angelia-server--inbuf))
            (setq angelia-server--inbuf (cdr extracted))
            (condition-case parse-err
                (let ((parsed (json-parse-string
                               (decode-coding-string (car extracted) 'utf-8 t)
                               :object-type 'hash-table
                               :null-object :null
                               :false-object :false)))
                  (angelia-server--dispatch conn parsed))
              (json-parse-error
               (angelia-server--log-error parse-err)
               (angelia-server--write-frame
                (angelia-server--make-error nil -32700
                                            (format "Parse error: %s"
                                                    (error-message-string parse-err)))))))
        (angelia-server-protocol-error
         (angelia-server--log-error err)
         (setq angelia-server--inbuf (unibyte-string))
         (setq angelia-server--quit-flag t))))))

(defun angelia-server--make-sentinel ()
  "Return a sentinel that flips the quit flag when stdin closes."
  (lambda (_proc event)
    (angelia-server--log "stdin reader sentinel: %s" (string-trim event))
    (when (or (string-prefix-p "finished" event)
              (string-prefix-p "exited" event)
              (string-prefix-p "deleted" event)
              (string-prefix-p "broken" event)
              (string-prefix-p "killed" event))
      (setq angelia-server--quit-flag t))))

;; ---------------------------------------------------------------------------
;; Source SHA1 (used for the version handshake).

(let ((path (or load-file-name buffer-file-name)))
  (when (and path (file-readable-p path))
    (setq angelia-server--source-sha1
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (secure-hash 'sha1 (current-buffer))))))

;; ---------------------------------------------------------------------------
;; Built-in handlers.

(defun angelia-server--builtin-ping (_conn _params)
  "Reply with a `pong' marker and current timestamp."
  (let ((h (make-hash-table :test #'equal)))
    (puthash "pong" t h)
    (puthash "timestamp" (format-time-string "%FT%T.%3N%z") h)
    h))

(defun angelia-server--builtin-version (_conn _params)
  "Reply with the server source SHA1, Emacs version, and PID."
  (let ((h (make-hash-table :test #'equal)))
    (puthash "sha1" (or angelia-server--source-sha1 :null) h)
    (puthash "emacs_version" emacs-version h)
    (puthash "pid" (emacs-pid) h)
    h))

(defun angelia-server--builtin-info (_conn _params)
  "Reply with version + uptime + PID + hostname + available backends."
  (angelia-server--init-proc-backends)
  (let ((h (make-hash-table :test #'equal)))
    (puthash "sha1" (or angelia-server--source-sha1 :null) h)
    (puthash "emacs_version" emacs-version h)
    (puthash "pid" (emacs-pid) h)
    (puthash "hostname" (or (system-name) "") h)
    (puthash "uptime_ms"
             (if angelia-server--start-time
                 (round (* 1000 (float-time
                                 (time-subtract (current-time)
                                                angelia-server--start-time))))
               0)
             h)
    (puthash "available_backends"
             (vconcat (mapcar #'car angelia-server--proc-backends))
             h)
    h))

(defun angelia-server--builtin-session-close (conn params)
  "Client-initiated close of the session named in PARAMS.
Tears down both server-side state and any per-feature resources (write-stream
temp file cleanup hooks etc. -- not yet wired) and emits the terminal `end'
event.  Returns t."
  (let ((session (and (hash-table-p params) (gethash "session" params))))
    (unless (stringp session)
      (error "session/close: missing or non-string `session' parameter"))
    (angelia-server--end-session conn session)
    t))

(defun angelia-server--builtin-session-echo (conn params)
  "Open a session and emit COUNT events with PAYLOAD, then end.
This is the canonical test driver for the session machinery -- not part of
the user-facing API, but kept always registered because it costs nothing
and lets the ERT suite cover the streaming path without a stand-in.

PARAMS keys (all optional):
  count            integer, number of `echo' events to emit (default 1)
  payload          string, copied verbatim into each event (default \"\")
  end-immediately  boolean, when true emit `end' with no `echo' events

Events emitted are deferred via `run-at-time' so the JSON-RPC response
carrying the new session id reaches the client BEFORE the events do --
otherwise the client would receive events for a session it hasn't
registered a callback for yet, and drop them."
  (let* ((session (angelia-server--make-session-id))
         (count (or (and (hash-table-p params) (gethash "count" params)) 1))
         (payload (or (and (hash-table-p params) (gethash "payload" params)) ""))
         (end-now (and (hash-table-p params)
                       (let ((v (gethash "end-immediately" params)))
                         (and v (not (eq v :false))))))
         (result (make-hash-table :test #'equal)))
    (angelia-server--register-session session (list :kind 'echo))
    (puthash "session" session result)
    (run-at-time
     0.005 nil
     (lambda ()
       (unless end-now
         (dotimes (i count)
           (let ((ev (make-hash-table :test #'equal)))
             (puthash "index" i ev)
             (puthash "payload" payload ev)
             (angelia-server--send-session-event conn session "echo" ev))))
       (angelia-server--end-session conn session)))
    result))

(angelia-server-register-method "server/ping"    #'angelia-server--builtin-ping)
(angelia-server-register-method "server/version" #'angelia-server--builtin-version)
(angelia-server-register-method "server/info"    #'angelia-server--builtin-info)
(angelia-server-register-method "session/close"  #'angelia-server--builtin-session-close)
(angelia-server-register-method "session/echo"   #'angelia-server--builtin-session-echo)

;; ---------------------------------------------------------------------------
;; File operation handlers.

(defun angelia-server--require-string-path (method params)
  "Extract and validate the \"path\" entry of PARAMS as a string.
METHOD is the calling method name (for error reporting only)."
  (let ((path (and (hash-table-p params) (gethash "path" params))))
    (unless (stringp path)
      (error "%s: missing or non-string `path' parameter" method))
    (expand-file-name path)))

(defun angelia-server--attr-type (attrs)
  "Return \"file\", \"directory\", or \"symlink\" given Emacs ATTRS."
  (let ((t0 (car attrs)))
    (cond ((stringp t0) "symlink")
          (t0           "directory")
          (t            "file"))))

(defun angelia-server--attrs-hash (attrs)
  "Return a hash-table view of the relevant fields of ATTRS."
  (let ((h (make-hash-table :test #'equal)))
    (puthash "type"  (angelia-server--attr-type attrs) h)
    (puthash "size"  (file-attribute-size attrs) h)
    (puthash "mtime" (format-time-string "%FT%T.%3N%z"
                                         (file-attribute-modification-time attrs))
             h)
    (puthash "mode"  (file-attribute-modes attrs) h)
    h))

(defconst angelia-server--default-chunk-size (* 64 1024)
  "Default chunk size in bytes for streaming reads and writes.")

(defun angelia-server--file-read (conn params)
  "Streamed read.  Returns `{session, size}'; the file bytes follow as a
sequence of `session/event {kind: \"chunk\", data: STR-base64}'
notifications terminated by `kind: \"end\"'.  Optional `chunk-size'
overrides `angelia-server--default-chunk-size'.

Emission is deferred via `run-at-time' so the JSON-RPC response reaches
the client BEFORE its chunks do; otherwise the client would receive
events for a session it has not yet registered a callback for."
  (let* ((path (angelia-server--require-string-path "file/read" params))
         (chunk-size (or (and (hash-table-p params)
                              (gethash "chunk-size" params))
                         angelia-server--default-chunk-size))
         (attrs (file-attributes path))
         (size (and attrs (file-attribute-size attrs))))
    (unless attrs
      (error "file/read: not found: %s" path))
    (let ((session (angelia-server--make-session-id))
          (result (make-hash-table :test #'equal)))
      (angelia-server--register-session
       session (list :kind 'file-read :path path :size size))
      (puthash "session" session result)
      (puthash "size" size result)
      (run-at-time
       0.005 nil
       (lambda ()
         (condition-case err
             (let ((offset 0))
               (while (< offset size)
                 (let* ((end (min size (+ offset chunk-size)))
                        (bytes (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally
                                  path nil offset end)
                                 (buffer-string)))
                        (payload (make-hash-table :test #'equal)))
                   (puthash "data" (base64-encode-string bytes t) payload)
                   (angelia-server--send-session-event
                    conn session "chunk" payload)
                   (setq offset end)))
               (angelia-server--end-session conn session))
           (error
            (angelia-server--log-error err)
            (let ((p (make-hash-table :test #'equal)))
              (puthash "message" (error-message-string err) p)
              (angelia-server--send-session-event
               conn session "error" p))
            (angelia-server--end-session conn session)))))
      result)))

(defun angelia-server--file-write-open (_conn params)
  "Open an atomic chunked write to PARAMS->path of declared PARAMS->size bytes.
Returns `{session}'.  Bytes are buffered into a sibling temp file in the
target's directory; `file/write-finish' renames it into place atomically.
If the session is closed without finishing, `--cleanup-session' deletes
the temp file."
  (let* ((path (angelia-server--require-string-path "file/write-open" params))
         (size (and (hash-table-p params) (gethash "size" params)))
         (dir (file-name-directory path)))
    (unless (integerp size)
      (error "file/write-open: missing integer `size' parameter"))
    (unless (file-directory-p dir)
      (error "file/write-open: parent directory does not exist: %s" dir))
    (let* ((tmp (make-temp-file
                 (expand-file-name ".angelia-write-" dir)))
           (session (angelia-server--make-session-id))
           (result (make-hash-table :test #'equal)))
      (angelia-server--register-session
       session
       (list :kind 'file-write
             :path path
             :tmp tmp
             :expected-size size
             :written-so-far 0))
      (puthash "session" session result)
      result)))

(defun angelia-server--file-write-chunk (_conn params)
  "Append PARAMS->data (base64) to the open write session PARAMS->session.
Returns `{accepted: N}' where N is the running byte count."
  (let* ((session (and (hash-table-p params) (gethash "session" params)))
         (b64 (and (hash-table-p params) (gethash "data" params)))
         (state (and session (gethash session angelia-server--sessions))))
    (unless (and (stringp session) state
                 (eq (plist-get state :kind) 'file-write))
      (error "file/write-chunk: unknown or wrong-kind session: %S" session))
    (unless (stringp b64)
      (error "file/write-chunk: missing string `data' parameter"))
    (let* ((bytes (base64-decode-string b64))
           (tmp (plist-get state :tmp))
           (running (+ (plist-get state :written-so-far) (length bytes)))
           (coding-system-for-write 'binary))
      (write-region bytes nil tmp t 'silent)
      ;; plist-put mutates in place for existing keys, but we re-store the
      ;; (possibly new) head defensively.
      (puthash session
               (plist-put state :written-so-far running)
               angelia-server--sessions)
      (let ((h (make-hash-table :test #'equal)))
        (puthash "accepted" running h)
        h))))

(defun angelia-server--file-write-finish (conn params)
  "Atomically rename the tmp file for PARAMS->session into its target path.
Ends the session (emitting `kind: \"end\"') and returns `{written: N}'.
If the running byte count differs from the declared size, signals an
error and leaves the tmp file in place for `--cleanup-session' to drop."
  (let* ((session (and (hash-table-p params) (gethash "session" params)))
         (state (and session (gethash session angelia-server--sessions))))
    (unless (and (stringp session) state
                 (eq (plist-get state :kind) 'file-write))
      (error "file/write-finish: unknown or wrong-kind session: %S" session))
    (let ((tmp (plist-get state :tmp))
          (path (plist-get state :path))
          (expected (plist-get state :expected-size))
          (written (plist-get state :written-so-far)))
      (unless (equal expected written)
        (error "file/write-finish: size mismatch (declared=%S written=%S)"
               expected written))
      ;; `rename-file' with OK-IF-ALREADY-EXISTS=t overwrites atomically.
      (rename-file tmp path t)
      ;; tmp is gone; clear it from state so cleanup doesn't try to delete
      ;; the now-renamed target file.
      (puthash session
               (plist-put state :tmp nil)
               angelia-server--sessions)
      (angelia-server--end-session conn session)
      (let ((h (make-hash-table :test #'equal)))
        (puthash "written" written h)
        h))))

(defun angelia-server--file-exists (_conn params)
  "Return t when PARAMS->path exists, nil otherwise."
  (let ((path (angelia-server--require-string-path "file/exists" params)))
    (if (file-exists-p path) t nil)))

(defun angelia-server--file-directory-p (_conn params)
  "Return t when PARAMS->path is a directory, nil otherwise."
  (let ((path (angelia-server--require-string-path "file/directory-p" params)))
    (if (file-directory-p path) t nil)))

(defun angelia-server--file-attributes (_conn params)
  "Return a hash of stat info for PARAMS->path."
  (let* ((path (angelia-server--require-string-path "file/attributes" params))
         (attrs (file-attributes path 'string)))
    (unless attrs (error "file/attributes: not found: %s" path))
    (angelia-server--attrs-hash attrs)))

(defun angelia-server--file-list-dir (_conn params)
  "List PARAMS->path.  Returns {entries: [{name,type,size}...]}."
  (let* ((path (angelia-server--require-string-path "file/list-dir" params))
         (names (directory-files path nil nil 'nosort))
         (entries
          (mapcar
           (lambda (name)
             (let* ((full (expand-file-name name path))
                    (attrs (file-attributes full))
                    (h (make-hash-table :test #'equal)))
               (puthash "name" name h)
               (puthash "type"
                        (if attrs (angelia-server--attr-type attrs) "file")
                        h)
               (when (and attrs (file-attribute-size attrs))
                 (puthash "size" (file-attribute-size attrs) h))
               h))
           names))
         (result (make-hash-table :test #'equal)))
    (puthash "entries" (vconcat entries) result)
    result))

(defun angelia-server--file-mkdir (_conn params)
  "Create the directory PARAMS->path.
When PARAMS->parents is non-nil, create intermediate directories too.
Returns t."
  (let ((path (angelia-server--require-string-path "file/mkdir" params))
        (parents (and (hash-table-p params) (gethash "parents" params))))
    (make-directory path (and parents (not (eq parents :json-false))))
    t))

(defun angelia-server--file-delete (_conn params)
  "Delete the file PARAMS->path.  Returns t."
  (let ((path (angelia-server--require-string-path "file/delete" params)))
    (delete-file path)
    t))

(defun angelia-server--file-delete-directory (_conn params)
  "Delete the directory PARAMS->path.
When PARAMS->recursive is non-nil and not `:false', also delete contents.
Returns t."
  (let ((path (angelia-server--require-string-path "file/delete-directory" params))
        (recursive (and (hash-table-p params) (gethash "recursive" params))))
    (delete-directory path (and recursive (not (eq recursive :false))))
    t))

(defun angelia-server--file-copy (_conn params)
  "Copy PARAMS->from to PARAMS->to.
PARAMS->ok-if-exists controls overwrite (default nil).  Returns t."
  (let ((from (and (hash-table-p params) (gethash "from" params)))
        (to (and (hash-table-p params) (gethash "to" params)))
        (ok (and (hash-table-p params) (gethash "ok-if-exists" params))))
    (unless (stringp from) (error "file/copy: missing string `from'"))
    (unless (stringp to)   (error "file/copy: missing string `to'"))
    (copy-file (expand-file-name from) (expand-file-name to)
               (and ok (not (eq ok :false)))
               t t t)
    t))

(defun angelia-server--file-rename (_conn params)
  "Rename PARAMS->from to PARAMS->to.
PARAMS->ok-if-exists controls overwrite (default nil).  Returns t."
  (let ((from (and (hash-table-p params) (gethash "from" params)))
        (to (and (hash-table-p params) (gethash "to" params)))
        (ok (and (hash-table-p params) (gethash "ok-if-exists" params))))
    (unless (stringp from) (error "file/rename: missing string `from'"))
    (unless (stringp to)   (error "file/rename: missing string `to'"))
    (rename-file (expand-file-name from) (expand-file-name to)
                 (and ok (not (eq ok :false))))
    t))

(defun angelia-server--file-symlink-target (_conn params)
  "Return the link target of PARAMS->path, or :null if PATH isn't a symlink."
  (let* ((path (angelia-server--require-string-path "file/symlink-target" params))
         (target (file-symlink-p path)))
    (if (stringp target) target :null)))

(defun angelia-server--file-make-symlink (_conn params)
  "Create symlink at PARAMS->linkpath pointing to PARAMS->target.  Returns t."
  (let ((target (and (hash-table-p params) (gethash "target" params)))
        (linkpath (and (hash-table-p params) (gethash "linkpath" params)))
        (ok (and (hash-table-p params) (gethash "ok-if-exists" params))))
    (unless (and (stringp target) (stringp linkpath))
      (error "file/symlink: missing string `target' or `linkpath'"))
    (make-symbolic-link target (expand-file-name linkpath)
                        (and ok (not (eq ok :false))))
    t))

(defun angelia-server--file-set-modes (_conn params)
  "Set the file modes of PARAMS->path to PARAMS->mode (integer).  Returns t."
  (let ((path (angelia-server--require-string-path "file/set-modes" params))
        (mode (and (hash-table-p params) (gethash "mode" params))))
    (unless (integerp mode)
      (error "file/set-modes: missing integer `mode'"))
    (set-file-modes path mode)
    t))

(defun angelia-server--file-set-times (_conn params)
  "Set the mtime of PARAMS->path to PARAMS->time (seconds since epoch).
nil time means now.  Returns t."
  (let* ((path (angelia-server--require-string-path "file/set-times" params))
         (time (and (hash-table-p params) (gethash "time" params)))
         (time-val (cond ((or (null time) (eq time :null)) nil)
                         ((numberp time) (seconds-to-time time))
                         (t (error "file/set-times: time must be a number")))))
    (set-file-times path time-val)
    t))

(defun angelia-server--file-writable-p (_conn params)
  "Return t if PARAMS->path is writable (real `access(W_OK)' check)."
  (let ((path (angelia-server--require-string-path "file/writable-p" params)))
    (if (file-writable-p path) t nil)))

(defun angelia-server--file-executable-p (_conn params)
  "Return t if PARAMS->path is executable."
  (let ((path (angelia-server--require-string-path "file/executable-p" params)))
    (if (file-executable-p path) t nil)))

(defun angelia-server--file-list-dir-attrs (_conn params)
  "List PARAMS->path with full attributes per entry.
Returns {entries: [{name, type, size, mode, mtime}, ...]}."
  (let* ((path (angelia-server--require-string-path "file/list-dir-attrs" params))
         (entries (directory-files-and-attributes path nil nil 'nosort 'string))
         (result-entries
          (mapcar
           (lambda (e)
             (let* ((name (car e))
                    (attrs (cdr e))
                    (h (make-hash-table :test #'equal)))
               (puthash "name" name h)
               (puthash "type"  (angelia-server--attr-type attrs) h)
               (when (file-attribute-size attrs)
                 (puthash "size" (file-attribute-size attrs) h))
               (puthash "mode"  (file-attribute-modes attrs) h)
               (puthash "mtime" (format-time-string
                                 "%FT%T.%3N%z"
                                 (file-attribute-modification-time attrs))
                        h)
               h))
           entries))
         (result (make-hash-table :test #'equal)))
    (puthash "entries" (vconcat result-entries) result)
    result))

(defun angelia-server--file-completions (_conn params)
  "Return `{names: [...]}' completion candidates against PARAMS->path.
PARAMS->file is the prefix being completed (default \"\")."
  (let* ((path (angelia-server--require-string-path "file/completions" params))
         (file (or (and (hash-table-p params) (gethash "file" params)) ""))
         (matches (file-name-all-completions
                   file (file-name-as-directory path)))
         (result (make-hash-table :test #'equal)))
    (puthash "names" (vconcat matches) result)
    result))

(defun angelia-server--watch-flags (flag-strs)
  "Map FLAG-STRS (a vector/list of strings) to `file-notify-add-watch' flags.
Recognises \"change\" and \"attribute-change\"; defaults to (change)."
  (let ((flags '()))
    (when (or (vectorp flag-strs) (listp flag-strs))
      (mapc (lambda (s)
              (cond ((equal s "change") (push 'change flags))
                    ((equal s "attribute-change") (push 'attribute-change flags))))
            (append flag-strs nil)))
    (or flags '(change))))

(defun angelia-server--emit-fsevent (conn session action file)
  "Push an `fsevent' session event: ACTION (a string) on FILE's basename."
  (let ((payload (make-hash-table :test #'equal)))
    (puthash "action" action payload)
    (puthash "file" (file-name-nondirectory (or file "")) payload)
    (angelia-server--send-session-event conn session "fsevent" payload)))

(defun angelia-server--make-watch-callback (conn session)
  "Return a `file-notify' callback forwarding events on SESSION to CONN.
Each event becomes a `kind: \"fsevent\"' carrying the basename of the changed
file and the action string.  `renamed' is split into a `deleted' of the old
name plus a `created' of the new, so the client's `file-notify-callback' (which
drops a bare `renamed') still sees both halves.  `stopped' ends the session."
  (lambda (event)
    ;; EVENT = (DESCRIPTOR ACTION FILE [FILE1]).
    (let ((action (nth 1 event))
          (file   (nth 2 event))
          (file1  (nth 3 event)))
      (condition-case err
          (pcase action
            ('stopped (angelia-server--end-session conn session))
            ('renamed
             (angelia-server--emit-fsevent conn session "deleted" file)
             (when file1
               (angelia-server--emit-fsevent conn session "created" file1)))
            (_ (angelia-server--emit-fsevent
                conn session (symbol-name action) file)))
        (error (angelia-server--log-error err))))))

(defun angelia-server--file-watch (conn params)
  "Watch PARAMS->path (a directory) and stream changes as session events.
Returns {session}.  PARAMS->flags is an array of strings (\"change\",
\"attribute-change\"); defaults to (\"change\").  The client always passes a
directory (filenotify reduces a file watch to its parent) and filters by
basename on its side, so this watches the whole directory."
  (let* ((path (angelia-server--require-string-path "file/watch" params))
         (flag-strs (and (hash-table-p params) (gethash "flags" params)))
         (flags (angelia-server--watch-flags flag-strs))
         (session (angelia-server--make-session-id))
         (result (make-hash-table :test #'equal))
         (desc (file-notify-add-watch
                path flags
                (angelia-server--make-watch-callback conn session))))
    (angelia-server--register-session
     session (list :kind 'file-watch :descriptor desc :path path))
    (angelia-server--log "file/watch: session=%s path=%s flags=%S"
                         session path flags)
    (puthash "session" session result)
    result))

(defun angelia-server--file-unwatch (conn params)
  "Stop the watch identified by PARAMS->session.  Returns t.
`angelia-server--end-session' runs the `file-watch' cleanup (rm-watch) and
emits the terminal `end' event."
  (let ((session (and (hash-table-p params) (gethash "session" params))))
    (unless (stringp session) (error "file/unwatch: missing string `session'"))
    (angelia-server--end-session conn session)
    t))

(angelia-server-register-method "file/read"             #'angelia-server--file-read)
(angelia-server-register-method "file/write-open"       #'angelia-server--file-write-open)
(angelia-server-register-method "file/write-chunk"      #'angelia-server--file-write-chunk)
(angelia-server-register-method "file/write-finish"     #'angelia-server--file-write-finish)
(angelia-server-register-method "file/exists"           #'angelia-server--file-exists)
(angelia-server-register-method "file/directory-p"      #'angelia-server--file-directory-p)
(angelia-server-register-method "file/attributes"       #'angelia-server--file-attributes)
(angelia-server-register-method "file/list-dir"         #'angelia-server--file-list-dir)
(angelia-server-register-method "file/list-dir-attrs"   #'angelia-server--file-list-dir-attrs)
(angelia-server-register-method "file/mkdir"            #'angelia-server--file-mkdir)
(angelia-server-register-method "file/delete"           #'angelia-server--file-delete)
(angelia-server-register-method "file/delete-directory" #'angelia-server--file-delete-directory)
(angelia-server-register-method "file/copy"             #'angelia-server--file-copy)
(angelia-server-register-method "file/rename"           #'angelia-server--file-rename)
(angelia-server-register-method "file/symlink-target"   #'angelia-server--file-symlink-target)
(angelia-server-register-method "file/symlink"          #'angelia-server--file-make-symlink)
(angelia-server-register-method "file/set-modes"        #'angelia-server--file-set-modes)
(angelia-server-register-method "file/set-times"        #'angelia-server--file-set-times)
(angelia-server-register-method "file/writable-p"       #'angelia-server--file-writable-p)
(angelia-server-register-method "file/executable-p"     #'angelia-server--file-executable-p)
(angelia-server-register-method "file/completions"      #'angelia-server--file-completions)
(angelia-server-register-method "file/watch"            #'angelia-server--file-watch)
(angelia-server-register-method "file/unwatch"          #'angelia-server--file-unwatch)

;; ---------------------------------------------------------------------------
;; Remote process / PTY handlers.

(defconst angelia-server--proc-allowed-signals
  '("TERM" "KILL" "INT" "HUP" "QUIT")
  "Signal names the wire protocol accepts; mapped to SIGTERM/SIGKILL/etc.")

(defun angelia-server--proc-require-session (method params)
  "Look up PARAMS->session as a `proc' session.  Signal error if not present."
  (let* ((session (and (hash-table-p params) (gethash "session" params)))
         (state (and session (gethash session angelia-server--sessions))))
    (unless (and state (eq (plist-get state :kind) 'proc))
      (error "%s: unknown or wrong-kind session: %S" method session))
    (cons session state)))

(defun angelia-server--proc-make-filter (conn session)
  "Return a process filter that forwards bytes as session/event kind=output."
  (lambda (_proc bytes)
    (let ((p (make-hash-table :test #'equal)))
      (puthash "data" (base64-encode-string bytes t) p)
      (angelia-server--send-session-event conn session "output" p))))

(defun angelia-server--proc-extract-signal-name (event)
  "Parse the signal name out of EVENT (a sentinel event string).
Recognises both the C-style upper-case forms (`SIGTERM', `TERM',
`\"killed by signal SIGTERM\"') AND Emacs's own lower-case sentinel
strings (`interrupt', `terminated', `hangup', ...) -- the lower-case
forms are what Emacs actually emits in batch mode for most signals,
so without these branches `:signal' came back as nil and clients had
to substring-match the event field for hints."
  (let ((e (string-trim event)))
    (cond
     ((string-match "killed by signal[^A-Z]*\\([A-Z]+\\)" e)
      (let ((s (match-string 1 e)))
        (if (string-prefix-p "SIG" s) (substring s 3) s)))
     ((string-match "\\bSIG\\([A-Z]+\\)\\b" e)
      (match-string 1 e))
     ((string-match "\\b\\(TERM\\|KILL\\|INT\\|HUP\\|QUIT\\|PIPE\\)\\b" e)
      (match-string 1 e))
     ;; Emacs's own lower-case sentinel phrasing.
     ((string-match-p "\\`interrupt" e)   "INT")
     ((string-match-p "\\`terminated" e)  "TERM")
     ((string-match-p "\\`killed" e)      "KILL")
     ((string-match-p "\\`hangup" e)      "HUP")
     ((string-match-p "\\`quit" e)        "QUIT")
     ((string-match-p "broken pipe" e)    "PIPE"))))

(defun angelia-server--proc-make-sentinel (conn session)
  "Return a sentinel that emits kind=exit then ends the session on death."
  (lambda (proc event)
    (let ((status (process-status proc)))
      (when (memq status '(exit signal failed closed))
        (let* ((exit-code (process-exit-status proc))
               (signal-name (angelia-server--proc-extract-signal-name event))
               (p (make-hash-table :test #'equal)))
          (puthash "code" (if (eq status 'exit) exit-code :null) p)
          (puthash "signal" (or signal-name :null) p)
          (puthash "event" (string-trim event) p)
          (angelia-server--send-session-event conn session "exit" p))
        (angelia-server--end-session conn session)))))

(defun angelia-server--proc-start (conn params)
  "Spawn a PTY-backed remote process and stream its output as session events.
Returns `{session, pid}'.  Recognised params:
  argv  vector of strings -- argv[0] is the program, rest are args.
  cwd   optional working directory.
  env   optional hash-table merged into `process-environment'.
  rows  optional initial PTY rows.
  cols  optional initial PTY columns."
  (let* ((argv (and (hash-table-p params) (gethash "argv" params)))
         (cwd  (and (hash-table-p params) (gethash "cwd" params)))
         (env  (and (hash-table-p params) (gethash "env" params)))
         (rows (and (hash-table-p params) (gethash "rows" params)))
         (cols (and (hash-table-p params) (gethash "cols" params)))
         (persist (and (hash-table-p params) (gethash "persist" params)))
         (backend-name (and (hash-table-p params) (gethash "backend" params)))
         (raw-argv (append argv nil))
         (effective-backend nil))
    (unless (and (vectorp argv) (> (length argv) 0)
                 (cl-every #'stringp raw-argv))
      (error "proc/start: `argv' must be a non-empty vector of strings"))
    ;; If persistence was requested, swap argv for the backend-wrapped form.
    (when (stringp persist)
      (let* ((chosen-name (or backend-name
                              (angelia-server--default-backend-name)))
             (backend (and chosen-name
                           (angelia-server--backend-by-name chosen-name))))
        (unless backend
          (error "proc/start: no persistence backend available (requested=%S, installed=%S)"
                 backend-name
                 (mapcar #'car (or angelia-server--proc-backends
                                   (progn (angelia-server--init-proc-backends)
                                          angelia-server--proc-backends)))))
        (setq raw-argv (funcall (angelia-server--proc-backend-wrap-argv backend)
                                raw-argv persist)
              effective-backend chosen-name)))
    (let* ((session (angelia-server--make-session-id))
           (process-environment
            (if (hash-table-p env)
                (let ((envs (copy-sequence process-environment)))
                  (maphash (lambda (k v)
                             (push (format "%s=%s" k v) envs))
                           env)
                  envs)
              process-environment))
           (default-directory (or (and (stringp cwd) (file-name-as-directory cwd))
                                  default-directory))
           (proc (make-process
                  :name (format "angelia-pty-%s" session)
                  :command raw-argv
                  :coding 'binary
                  :connection-type 'pty
                  :noquery t
                  :filter (angelia-server--proc-make-filter conn session)
                  :sentinel (angelia-server--proc-make-sentinel conn session))))
      (when (and (integerp rows) (integerp cols))
        (ignore-errors (set-process-window-size proc rows cols)))
      (angelia-server--register-session
       session (list :kind 'proc :process proc
                     :persist persist :backend effective-backend))
      (let ((result (make-hash-table :test #'equal)))
        (puthash "session" session result)
        (puthash "pid" (process-id proc) result)
        (when effective-backend
          (puthash "backend" effective-backend result))
        result))))

(defun angelia-server--proc-input (_conn params)
  "Decode PARAMS->data (base64) and write it to the PTY's stdin.
Returns `{accepted: N}'."
  (let* ((cell (angelia-server--proc-require-session "proc/input" params))
         (state (cdr cell))
         (b64 (and (hash-table-p params) (gethash "data" params))))
    (unless (stringp b64)
      (error "proc/input: missing string `data'"))
    (let* ((bytes (base64-decode-string b64))
           (proc (plist-get state :process)))
      (process-send-string proc bytes)
      (let ((h (make-hash-table :test #'equal)))
        (puthash "accepted" (length bytes) h)
        h))))

(defun angelia-server--proc-resize (_conn params)
  "Resize the PTY for PARAMS->session to PARAMS->rows by PARAMS->cols."
  (let* ((cell (angelia-server--proc-require-session "proc/resize" params))
         (state (cdr cell))
         (rows (and (hash-table-p params) (gethash "rows" params)))
         (cols (and (hash-table-p params) (gethash "cols" params))))
    (unless (and (integerp rows) (integerp cols))
      (error "proc/resize: rows/cols must be integers"))
    (set-process-window-size (plist-get state :process) rows cols)
    (make-hash-table :test #'equal)))

(defun angelia-server--proc-signal (_conn params)
  "Send PARAMS->signal (a short name like \"TERM\") to PARAMS->session's PID."
  (let* ((cell (angelia-server--proc-require-session "proc/signal" params))
         (state (cdr cell))
         (sig (and (hash-table-p params) (gethash "signal" params))))
    (unless (member sig angelia-server--proc-allowed-signals)
      (error "proc/signal: signal must be one of %S, got %S"
             angelia-server--proc-allowed-signals sig))
    (signal-process (process-id (plist-get state :process)) (intern sig))
    (make-hash-table :test #'equal)))

(defun angelia-server--proc-list-persisted (_conn params)
  "Return `{sessions: [{name, backend, alive}, ...]}' across one/all backends."
  (angelia-server--init-proc-backends)
  (let* ((requested (and (hash-table-p params) (gethash "backend" params)))
         (backends (if requested
                       (let ((b (assoc requested angelia-server--proc-backends)))
                         (and b (list b)))
                     angelia-server--proc-backends))
         (rows '()))
    (dolist (entry backends)
      (let ((name (car entry))
            (b (cdr entry)))
        (dolist (item (or (ignore-errors
                            (funcall (angelia-server--proc-backend-list-fn b)))
                          '()))
          (let ((h (make-hash-table :test #'equal)))
            (puthash "name" (car item) h)
            (puthash "backend" name h)
            (puthash "alive" (if (cadr item) t :false) h)
            (push h rows)))))
    (let ((result (make-hash-table :test #'equal)))
      (puthash "sessions" (vconcat (nreverse rows)) result)
      result)))

(defun angelia-server--proc-reattach (conn params)
  "Open a fresh PTY session re-entering the persisted PARAMS->name on BACKEND."
  (let* ((name (and (hash-table-p params) (gethash "name" params)))
         (backend-name (and (hash-table-p params) (gethash "backend" params)))
         (backend (and backend-name
                       (angelia-server--backend-by-name backend-name))))
    (unless (stringp name)
      (error "proc/reattach: missing string `name'"))
    (unless backend
      (error "proc/reattach: unknown backend: %S" backend-name))
    (let* ((argv (funcall (angelia-server--proc-backend-reattach backend) name))
           ;; Synthesize a `params' for proc/start that runs the reattach argv
           ;; directly (already wrapped; do NOT re-wrap via persist).
           (sub-params (make-hash-table :test #'equal)))
      (puthash "argv" (vconcat argv) sub-params)
      (let ((result (angelia-server--proc-start conn sub-params)))
        ;; Annotate the response with the backend + persist name so the
        ;; client can re-detach it later without remembering the original
        ;; call.
        (puthash "backend" backend-name result)
        (puthash "persist" name result)
        ;; Update the session row so cleanup/detach semantics behave
        ;; like a persisted spawn.
        (let* ((sid (gethash "session" result))
               (state (gethash sid angelia-server--sessions)))
          (when state
            (puthash sid
                     (plist-put (plist-put state :persist name)
                                :backend backend-name)
                     angelia-server--sessions)))
        result))))

(defun angelia-server--proc-detach (conn params)
  "Close the local-side PTY session PARAMS->session, leaving the persisted
process running.  Functionally identical to `session/close' -- the
connector (dtach/tmux/screen client) dies; the wrapped program survives
because it has been daemonised by its backend."
  (angelia-server--builtin-session-close conn params))

(defun angelia-server--proc-kill-persisted (_conn params)
  "Tear down persisted PARAMS->name in PARAMS->backend (calls backend kill-fn)."
  (let* ((name (and (hash-table-p params) (gethash "name" params)))
         (backend-name (and (hash-table-p params) (gethash "backend" params)))
         (backend (and backend-name
                       (angelia-server--backend-by-name backend-name))))
    (unless (stringp name)
      (error "proc/kill-persisted: missing string `name'"))
    (unless backend
      (error "proc/kill-persisted: unknown backend: %S" backend-name))
    (funcall (angelia-server--proc-backend-kill-fn backend) name)
    (make-hash-table :test #'equal)))

(defun angelia-server--proc-exec (conn params)
  "Spawn a non-PTY one-shot command on the remote and stream its output.
PARAMS keys (all but `argv' optional):
  argv   vector of strings -- argv[0] is the program, rest are args.
  cwd    working directory.
  env    hash-table of extra environment variables.
  stdin  base64-encoded bytes written to the process's stdin and EOF'd.

Returns `{session, pid}'.  Emits these `session/event' notifications:
  kind=stdout {data: BASE64}
  kind=stderr {data: BASE64}
  kind=exit   {code: N|null, signal: STR|null, event: STR}
and finally the terminal `kind=end' from `--end-session'.

Modelled on `proc/start' but with `:connection-type 'pipe' so no PTY is
allocated; stderr is captured via a `make-pipe-process' stderr sink so
the two streams can be reported separately."
  (let* ((argv (and (hash-table-p params) (gethash "argv" params)))
         (cwd  (and (hash-table-p params) (gethash "cwd" params)))
         (env  (and (hash-table-p params) (gethash "env" params)))
         (stdin-b64 (and (hash-table-p params) (gethash "stdin" params)))
         (raw-argv (append argv nil)))
    (unless (and (vectorp argv) (> (length argv) 0)
                 (cl-every #'stringp raw-argv))
      (error "proc/exec: `argv' must be a non-empty vector of strings"))
    (let* ((session (angelia-server--make-session-id))
           (process-environment
            (if (hash-table-p env)
                (let ((envs (copy-sequence process-environment)))
                  (maphash (lambda (k v)
                             (push (format "%s=%s" k v) envs))
                           env)
                  envs)
              process-environment))
           (default-directory (or (and (stringp cwd)
                                       (file-name-as-directory cwd))
                                  default-directory))
           (stderr-pipe
            (make-pipe-process
             :name (format "angelia-exec-stderr-%s" session)
             :noquery t
             :coding 'binary
             :filter
             (lambda (_p bytes)
               (let ((p (make-hash-table :test #'equal)))
                 (puthash "data" (base64-encode-string bytes t) p)
                 (angelia-server--send-session-event
                  conn session "stderr" p)))))
           (proc
            (make-process
             :name (format "angelia-exec-%s" session)
             :command raw-argv
             :coding 'binary
             :connection-type 'pipe
             :noquery t
             :stderr stderr-pipe
             :filter
             (lambda (_p bytes)
               (let ((p (make-hash-table :test #'equal)))
                 (puthash "data" (base64-encode-string bytes t) p)
                 (angelia-server--send-session-event
                  conn session "stdout" p)))
             :sentinel
             (lambda (p event)
               (let ((status (process-status p)))
                 (when (memq status '(exit signal failed closed))
                   (let* ((exit-code (process-exit-status p))
                          (signal-name
                           (angelia-server--proc-extract-signal-name event))
                          (payload (make-hash-table :test #'equal)))
                     (puthash "code"
                              (if (eq status 'exit) exit-code :null)
                              payload)
                     (puthash "signal" (or signal-name :null) payload)
                     (puthash "event" (string-trim event) payload)
                     (angelia-server--send-session-event
                      conn session "exit" payload))
                   (angelia-server--end-session conn session)))))))
      (angelia-server--register-session
       session (list :kind 'proc-exec
                     :process proc
                     :stderr-pipe stderr-pipe))
      (when (stringp stdin-b64)
        (let ((bytes (base64-decode-string stdin-b64)))
          (when (> (length bytes) 0)
            (process-send-string proc bytes)))
        (process-send-eof proc))
      (let ((result (make-hash-table :test #'equal)))
        (puthash "session" session result)
        (puthash "pid" (process-id proc) result)
        result))))

(defun angelia-server--proc-exec-stdin (_conn params)
  "Write PARAMS->data (base64) to the stdin of PARAMS->session.
Optional PARAMS->eof, when non-nil and not :false, sends EOF after the bytes.
Returns `{accepted: N}'."
  (let* ((session (and (hash-table-p params) (gethash "session" params)))
         (state (and session (gethash session angelia-server--sessions)))
         (b64 (and (hash-table-p params) (gethash "data" params)))
         (eof (and (hash-table-p params) (gethash "eof" params))))
    (unless (and state (eq (plist-get state :kind) 'proc-exec))
      (error "proc/exec-stdin: unknown or wrong-kind session: %S" session))
    (let ((proc (plist-get state :process))
          (n 0))
      (when (stringp b64)
        (let ((bytes (base64-decode-string b64)))
          (when (> (length bytes) 0)
            (process-send-string proc bytes))
          (setq n (length bytes))))
      (when (and eof (not (eq eof :false)))
        (process-send-eof proc))
      (let ((h (make-hash-table :test #'equal)))
        (puthash "accepted" n h)
        h))))

(angelia-server-register-method "proc/start"          #'angelia-server--proc-start)
(angelia-server-register-method "proc/exec"           #'angelia-server--proc-exec)
(angelia-server-register-method "proc/exec-stdin"     #'angelia-server--proc-exec-stdin)
(angelia-server-register-method "proc/input"          #'angelia-server--proc-input)
(angelia-server-register-method "proc/resize"         #'angelia-server--proc-resize)
(angelia-server-register-method "proc/signal"         #'angelia-server--proc-signal)
(angelia-server-register-method "proc/list-persisted" #'angelia-server--proc-list-persisted)
(angelia-server-register-method "proc/reattach"       #'angelia-server--proc-reattach)
(angelia-server-register-method "proc/detach"         #'angelia-server--proc-detach)
(angelia-server-register-method "proc/kill-persisted" #'angelia-server--proc-kill-persisted)

;; ---------------------------------------------------------------------------
;; Process persistence -- pluggable dtach / tmux / screen backends.

(cl-defstruct (angelia-server--proc-backend
               (:constructor angelia-server--proc-backend-create))
  "Pluggable interface for one persistence backend (dtach / tmux / screen).
Each backend supplies a small set of pure-functional callbacks; the
session-level code in `proc/start' calls `wrap-argv' to obtain the
process command line, while `proc/list-persisted' / `proc/reattach' /
`proc/kill-persisted' route through the other slots."
  name              ; "dtach" / "tmux" / "screen"
  binary            ; "dtach" / "tmux" / "screen"
  wrap-argv         ; (lambda (argv name) -> wrapped argv list)
  reattach          ; (lambda (name) -> argv list that re-enters)
  list-fn           ; (lambda () -> list of (name alive-bool))
  kill-fn)          ; (lambda (name) -> ignored)

(defconst angelia-server--dtach-sock-dir
  (expand-file-name "~/.cache/angelia/dtach")
  "Directory holding the per-session dtach socket files.")

(defun angelia-server--dtach-sock (name)
  "Return the dtach socket path for persisted session NAME."
  (expand-file-name (concat name ".sock") angelia-server--dtach-sock-dir))

(defun angelia-server--make-backend-dtach ()
  "Return the dtach backend struct."
  (angelia-server--proc-backend-create
   :name "dtach"
   :binary "dtach"
   :wrap-argv
   (lambda (argv name)
     (make-directory angelia-server--dtach-sock-dir t)
     (append (list "dtach" "-A" (angelia-server--dtach-sock name)
                   "-z" "-E" "-r" "winch")
             argv))
   :reattach
   (lambda (name)
     (list "dtach" "-a" (angelia-server--dtach-sock name) "-r" "winch"))
   :list-fn
   (lambda ()
     (when (file-directory-p angelia-server--dtach-sock-dir)
       (let (result)
         (dolist (f (directory-files angelia-server--dtach-sock-dir
                                     nil "\\.sock\\'"))
           (push (list (file-name-sans-extension f) t) result))
         result)))
   :kill-fn
   (lambda (name)
     (let ((sock (angelia-server--dtach-sock name)))
       ;; dtach has no built-in "kill session"; find the daemon that owns
       ;; the socket by matching the command line, SIGTERM it, then remove
       ;; the socket file.  pkill is on every POSIX-ish system we care
       ;; about; if it isn't, the caller sees a non-zero exit and can
       ;; clean up manually.
       (when (file-exists-p sock)
         (call-process "pkill" nil nil nil "-f"
                       (regexp-quote sock))
         (ignore-errors (delete-file sock)))
       t))))

(defun angelia-server--tmux-session-name (name)
  "Return the tmux session name reserved for persisted NAME."
  (concat "angelia-" name))

(defun angelia-server--make-backend-tmux ()
  "Return the tmux backend struct.
The wrapper forces `TERM' to a known-good value because the server's
inherited TERM is typically `dumb' (no terminfo entry), and tmux refuses
to attach to a terminal it cannot clear."
  (angelia-server--proc-backend-create
   :name "tmux"
   :binary "tmux"
   :wrap-argv
   (lambda (argv name)
     (let ((sess (angelia-server--tmux-session-name name))
           (cmd-str (mapconcat #'shell-quote-argument argv " ")))
       (list "sh" "-c"
             (format
              "export TERM=${TERM:-xterm-256color}; tmux has-session -t %s 2>/dev/null || tmux new-session -d -s %s %s; exec tmux attach -t %s"
              (shell-quote-argument sess)
              (shell-quote-argument sess)
              cmd-str
              (shell-quote-argument sess)))))
   :reattach
   (lambda (name)
     (list "sh" "-c"
           (format "export TERM=${TERM:-xterm-256color}; exec tmux attach -t %s"
                   (shell-quote-argument
                    (angelia-server--tmux-session-name name)))))
   :list-fn
   (lambda ()
     (let (result)
       (with-temp-buffer
         (when (zerop (call-process
                       "tmux" nil t nil
                       "list-sessions" "-F"
                       "#{session_name}\t#{?session_attached,1,0}"))
           (goto-char (point-min))
           (while (re-search-forward "^angelia-\\([^\t]+\\)\t" nil t)
             (push (list (match-string 1) t) result))))
       result))
   :kill-fn
   (lambda (name)
     (call-process "tmux" nil nil nil "kill-session" "-t"
                   (angelia-server--tmux-session-name name)))))

(defun angelia-server--screen-session-name (name)
  "Return the screen session base name reserved for persisted NAME."
  (concat "angelia-" name))

(defun angelia-server--make-backend-screen ()
  "Return the GNU screen backend struct.
The wrapper forces `TERM' to a known-good value because the server's
inherited TERM is typically `dumb', which screen cannot drive."
  (angelia-server--proc-backend-create
   :name "screen"
   :binary "screen"
   :wrap-argv
   (lambda (argv name)
     (let ((sess (angelia-server--screen-session-name name))
           (cmd-str (mapconcat #'shell-quote-argument argv " ")))
       (list "sh" "-c"
             (format
              "export TERM=${TERM:-xterm-256color}; screen -ls %s >/dev/null 2>&1 || screen -dmS %s %s; exec screen -x %s"
              (shell-quote-argument sess)
              (shell-quote-argument sess)
              cmd-str
              (shell-quote-argument sess)))))
   :reattach
   (lambda (name)
     (list "sh" "-c"
           (format "export TERM=${TERM:-xterm-256color}; exec screen -x %s"
                   (shell-quote-argument
                    (angelia-server--screen-session-name name)))))
   :list-fn
   (lambda ()
     (let (result)
       (with-temp-buffer
         (call-process "screen" nil t nil "-ls")
         (goto-char (point-min))
         (while (re-search-forward
                 "^[\t ]*\\(?:[0-9]+\\.\\)?angelia-\\([^[:space:]]+\\)"
                 nil t)
           (push (list (match-string 1) t) result)))
       result))
   :kill-fn
   (lambda (name)
     (call-process "screen" nil nil nil
                   "-S" (angelia-server--screen-session-name name)
                   "-X" "quit"))))

(defun angelia-server--init-proc-backends ()
  "Probe for dtach / tmux / screen and populate `angelia-server--proc-backends'.
Idempotent; safe to call repeatedly."
  (unless angelia-server--proc-backends
    (dolist (mk (list (cons "dtach"  #'angelia-server--make-backend-dtach)
                      (cons "tmux"   #'angelia-server--make-backend-tmux)
                      (cons "screen" #'angelia-server--make-backend-screen)))
      (let* ((name (car mk))
             (b (funcall (cdr mk))))
        (when (executable-find (angelia-server--proc-backend-binary b))
          (push (cons name b) angelia-server--proc-backends))))
    (setq angelia-server--proc-backends
          (nreverse angelia-server--proc-backends))))

(defun angelia-server--backend-by-name (name)
  "Return the backend struct registered under NAME (a string), or nil."
  (angelia-server--init-proc-backends)
  (cdr (assoc name angelia-server--proc-backends)))

(defun angelia-server--default-backend-name ()
  "Return the name of the first available backend, dtach > tmux > screen."
  (angelia-server--init-proc-backends)
  (cl-some (lambda (n)
             (and (assoc n angelia-server--proc-backends) n))
           '("dtach" "tmux" "screen")))

;; ---------------------------------------------------------------------------
;; Entry point.

(defun angelia-server--stdin-path ()
  "Return a path that `cat' can open to read this server process's stdin.
Checks ANGELIA_STDIN_FIFO first (set by the client's shell wrapper for macOS
and other non-Linux hosts), then falls back to /proc/PID/fd/0 on Linux."
  (or (getenv "ANGELIA_STDIN_FIFO")
      (format "/proc/%d/fd/0" (emacs-pid))))

(defun angelia-server-main ()
  "Entry point invoked from `emacs --batch -l server.el -f angelia-server-main'.
Spawns a `cat' subprocess reading our stdin (via ANGELIA_STDIN_FIFO on macOS,
/proc/PID/fd/0 on Linux) so we can drive an async event loop with
`accept-process-output'.  Returns when stdin closes or `angelia-server--quit-flag'
is set."
  (setq angelia-server--start-time (current-time)
        angelia-server--inbuf (unibyte-string)
        angelia-server--quit-flag nil
        tempus-debug (and (getenv "ANGELIA_DEBUG") t)
        tempus-log-function #'angelia-server--log
        angelia-server--response-delay-ms
        (angelia-server--parse-delay-ms (getenv "ANGELIA_DELAY_MS")))
  (angelia-server--log "startup: pid=%d sha1=%s emacs=%s host=%s debug=%s delay=%dms"
                       (emacs-pid)
                       (or angelia-server--source-sha1 "<unknown>")
                       emacs-version
                       (or (system-name) "?")
                       tempus-debug
                       angelia-server--response-delay-ms)
  (let* ((conn (angelia-server--conn-create))
         (proc (make-process
                :name "angelia-stdin"
                :command (list "cat" (angelia-server--stdin-path))
                :coding 'binary
                :connection-type 'pipe
                :noquery t
                :filter (angelia-server--make-filter conn)
                :sentinel (angelia-server--make-sentinel))))
    (setf (angelia-server--conn-proc conn) proc)
    (unwind-protect
        (while (and (not angelia-server--quit-flag)
                    (process-live-p proc))
          (accept-process-output proc 0.1))
      (when (process-live-p proc)
        (delete-process proc))
      (angelia-server--log "shutdown"))))

(provide 'angelia-server)
;;; angelia-server.el ends here
