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
;; Dispatch.

(defun angelia-server--dispatch (conn frame)
  "Dispatch FRAME (a parsed JSON-RPC envelope) through the method registry.
CONN is the connection context, passed verbatim to every handler."
  (let* ((id (gethash "id" frame))
         (method (gethash "method" frame))
         (params (gethash "params" frame))
         (handler (and method (gethash method angelia-server--methods))))
    (angelia-server--log "dispatch: method=%S id=%S handler=%S"
                         method id (and handler t))
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
      (let ((t0 (current-time)))
        (condition-case err
            (let ((result (funcall handler conn params)))
              (angelia-server--log "handler %s ok in %.1f ms" method
                                   (* 1000 (float-time
                                            (time-subtract (current-time) t0))))
              (when (angelia-server--has-id-p id)
                (angelia-server--write-frame
                 (angelia-server--make-result id result))))
          (error
           (angelia-server--log-error err)
           (when (angelia-server--has-id-p id)
             (angelia-server--write-frame
              (angelia-server--make-error id -32603
                                          (error-message-string err)))))))))))

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
  "Reply with version + uptime + PID + hostname diagnostics."
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
    h))

(angelia-server-register-method "server/ping"    #'angelia-server--builtin-ping)
(angelia-server-register-method "server/version" #'angelia-server--builtin-version)
(angelia-server-register-method "server/info"    #'angelia-server--builtin-info)

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

(defun angelia-server--file-read (_conn params)
  "Read PARAMS->path and return {content: base64(bytes), size: N}."
  (let ((path (angelia-server--require-string-path "file/read" params)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (let* ((bytes (buffer-string))
             (h     (make-hash-table :test #'equal)))
        (puthash "content" (base64-encode-string bytes t) h)
        (puthash "size"    (length bytes) h)
        h))))

(defun angelia-server--file-write (_conn params)
  "Decode PARAMS->content from base64 and write it to PARAMS->path.
Returns {written: N}."
  (let ((path (angelia-server--require-string-path "file/write" params))
        (b64  (and (hash-table-p params) (gethash "content" params))))
    (unless (stringp b64)
      (error "file/write: missing or non-string `content' parameter"))
    (let* ((bytes (base64-decode-string b64))
           (coding-system-for-write 'binary))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert bytes)
        (write-region (point-min) (point-max) path nil 'silent))
      (let ((h (make-hash-table :test #'equal)))
        (puthash "written" (length bytes) h)
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

(angelia-server-register-method "file/read"        #'angelia-server--file-read)
(angelia-server-register-method "file/write"       #'angelia-server--file-write)
(angelia-server-register-method "file/exists"      #'angelia-server--file-exists)
(angelia-server-register-method "file/directory-p" #'angelia-server--file-directory-p)
(angelia-server-register-method "file/attributes"  #'angelia-server--file-attributes)
(angelia-server-register-method "file/list-dir"    #'angelia-server--file-list-dir)
(angelia-server-register-method "file/mkdir"       #'angelia-server--file-mkdir)
(angelia-server-register-method "file/delete"      #'angelia-server--file-delete)

;; ---------------------------------------------------------------------------
;; Entry point.

(defun angelia-server-main ()
  "Entry point invoked from `emacs --batch -l server.el -f angelia-server-main'.
Spawns a `cat' subprocess that reads our own stdin (via /proc/PID/fd/0) so we
can drive an async event loop with `accept-process-output'.  Returns when
stdin closes or `angelia-server--quit-flag' is set."
  (setq angelia-server--start-time (current-time)
        angelia-server--inbuf (unibyte-string)
        angelia-server--quit-flag nil)
  (angelia-server--log "startup: pid=%d sha1=%s emacs=%s host=%s"
                       (emacs-pid)
                       (or angelia-server--source-sha1 "<unknown>")
                       emacs-version
                       (or (system-name) "?"))
  (let* ((conn (angelia-server--conn-create))
         (proc (make-process
                :name "angelia-stdin"
                :command (list "cat" (format "/proc/%d/fd/0" (emacs-pid)))
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
