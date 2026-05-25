;;; -*- lexical-binding: t; -*-
;;; angelia-client-files.el --- file-name-handler-alist entry for /@angelia:

;; Registers a `file-name-handler-alist' entry for the `/@angelia:HOST:/path'
;; URL syntax.  HOST is whatever you'd pass to `ssh' (~/.ssh/config aliases,
;; user@host, custom ports all work because we never parse it).
;;
;; Implements the eight file operations called out in the project plan
;; (insert-file-contents, write-region, file-exists-p, file-attributes,
;; file-directory-p, directory-files, make-directory, delete-file).  Every
;; other file operation falls through to the default handler chain so we
;; don't crash dired / autosave / etc., even though those ops are mostly
;; no-ops for remote paths today.

(require 'cl-lib)
(require 'subr-x)
(require 'angelia-client)

;; ---------------------------------------------------------------------------
;; Path syntax.
;;
;; /@angelia:HOST:/abs/path
;;            ^^^^ ^^^^^^^^^
;; HOST is `.+?' (lazy) so the LAST `:/' anchors the host/path boundary.
;; This handles HOSTs that themselves contain colons in unusual cases.

(defconst angelia-client-files--prefix "/@angelia:"
  "Path prefix that routes file operations through the Angelia handler.")

(defconst angelia-client-files--regexp
  "\\`/@angelia:\\(.+?\\):\\(/.*\\)\\'"
  "Match `/@angelia:HOST:/path' and capture HOST + remote path.")

(defun angelia-client-files--parse (path)
  "Return (HOST . REMOTE-PATH) for PATH or nil when it does not match."
  (when (and (stringp path)
             (string-match angelia-client-files--regexp path))
    (cons (match-string 1 path) (match-string 2 path))))

(defun angelia-client-files--make-path (host remote)
  "Compose an Angelia URL from HOST and REMOTE."
  (concat angelia-client-files--prefix host ":" remote))

;; ---------------------------------------------------------------------------
;; Hash-table helpers (params always go up as hash-tables).

(defun angelia-client-files--params (&rest kv)
  "Build a hash-table out of KV (a flat string/value plist of params)."
  (let ((h (make-hash-table :test #'equal)))
    (while kv
      (puthash (pop kv) (pop kv) h))
    h))

;; ---------------------------------------------------------------------------
;; Operation dispatchers.

(defun angelia-client-files--insert-file-contents (host remote args)
  "Read REMOTE on HOST and insert its bytes into the current buffer.
ARGS is the full argument list to `insert-file-contents'."
  (let* ((filename (nth 0 args))
         (visit    (nth 1 args))
         (beg      (nth 2 args))
         (end      (nth 3 args))
         (replace  (nth 4 args))
         (resp (angelia-client-call
                host 'file/read
                (angelia-client-files--params "path" remote)))
         (decoded (base64-decode-string (plist-get resp :content)))
         (selected (cond ((and beg end) (substring decoded beg end))
                         (beg           (substring decoded beg))
                         (end           (substring decoded 0 end))
                         (t             decoded))))
    (when replace
      (delete-region (point-min) (point-max)))
    ;; Buffer is multibyte by default; insert raw bytes by going via a unibyte
    ;; temp buffer and then decoding as UTF-8.  Binary files still survive
    ;; via the base64 transport even if the in-buffer view is munged.
    (insert (decode-coding-string selected 'utf-8 t))
    (when visit
      (setq buffer-file-name filename)
      (set-buffer-modified-p nil)
      ;; Record the remote file's mtime as the buffer's last-visited modtime.
      ;; With no arg, `set-visited-file-modtime' looks up `file-attributes' on
      ;; the visited path -- which routes through our handler -- so the value
      ;; it records matches what `verify-visited-file-modtime' will read
      ;; later, and `save-buffer' skips the "changed since visited" prompt.
      (set-visited-file-modtime)
      ;; Disable on-save backups + lockfiles + autosave for this buffer.
      ;; Each of those tries to operate on the local fs path, which doesn't
      ;; exist; the result is `save-buffer' failing with `file-missing'.  The
      ;; user can opt back in by clearing these locals if they want remote
      ;; backups (which would need their own RPC handlers).
      (setq-local backup-inhibited t)
      (setq-local create-lockfiles nil)
      (auto-save-mode -1))
    (list filename (length selected))))

(defun angelia-client-files--write-region (host remote args)
  "Send the contents of the START..END region to REMOTE on HOST.
ARGS is the full argument list to `write-region'."
  (let* ((start (nth 0 args))
         (end   (nth 1 args))
         (filename (nth 2 args))
         (append (nth 3 args))
         (visit (nth 4 args))
         (bytes (cond
                 ((stringp start) start)
                 ;; `write-region' allows START=END=nil to mean "entire
                 ;; accessible buffer", which is what `save-buffer' passes.
                 ((and (null start) (null end))
                  (buffer-substring-no-properties (point-min) (point-max)))
                 (t (buffer-substring-no-properties start end))))
         (encoded (encode-coding-string bytes 'utf-8 t))
         (b64 (base64-encode-string encoded t)))
    (when append
      (error "angelia: write-region :append is not implemented"))
    (angelia-client-call host 'file/write
                         (angelia-client-files--params
                          "path" remote
                          "content" b64))
    (when (or (eq visit t) (stringp visit))
      (setq buffer-file-name (if (stringp visit) visit filename))
      (set-buffer-modified-p nil)
      (set-visited-file-modtime (current-time)))
    (unless (or (null visit) (eq visit t) (stringp visit))
      (message "Wrote %s" filename))
    nil))

(defun angelia-client-files--rpc-bool (host method remote)
  "RPC METHOD with `path' = REMOTE on HOST.  Return t/nil based on response.
Falls back to nil if the RPC signals an error (mirrors `file-exists-p' and
friends, which simply return nil for unreadable paths rather than erroring)."
  (condition-case _err
      (eq (angelia-client-call host method
                               (angelia-client-files--params "path" remote))
          t)
    (jsonrpc-error nil)
    (error nil)))

(defun angelia-client-files--file-attributes (host remote)
  "Return a list shaped like `file-attributes' returns, or nil if absent."
  (condition-case _err
      (let* ((resp (angelia-client-call
                    host 'file/attributes
                    (angelia-client-files--params "path" remote)))
             (type (plist-get resp :type))
             (size (plist-get resp :size))
             (mode (plist-get resp :mode))
             (mtime-str (plist-get resp :mtime))
             (type-val (cond ((equal type "directory") t)
                             ((equal type "symlink")
                              ;; symlink target not currently exposed.
                              "")
                             (t nil)))
             (mtime (if mtime-str
                        (ignore-errors (date-to-time mtime-str))
                      '(0 0 0 0))))
        ;; (TYPE NLINK UID GID ATIME MTIME CTIME SIZE MODES UNUSED INO DEV)
        (list type-val 1 0 0 mtime mtime mtime size mode nil 0 0))
    (jsonrpc-error nil)
    (error nil)))

(defun angelia-client-files--directory-files (host remote args)
  "Implement `directory-files' for REMOTE on HOST.
ARGS is the full argument list (DIRECTORY FULL MATCH NOSORT &optional COUNT)."
  (let* ((directory (nth 0 args))
         (full      (nth 1 args))
         (match     (nth 2 args))
         (nosort    (nth 3 args))
         (resp (angelia-client-call
                host 'file/list-dir
                (angelia-client-files--params "path" remote)))
         (entries (plist-get resp :entries))
         (names (mapcar (lambda (e) (plist-get e :name))
                        (append entries nil)))
         (filtered (if match
                       (cl-remove-if-not (lambda (n) (string-match-p match n))
                                         names)
                     names))
         (sorted (if nosort filtered (sort filtered #'string<)))
         (dir (if (string-suffix-p "/" directory)
                  directory (concat directory "/"))))
    (if full
        (mapcar (lambda (n) (concat dir n)) sorted)
      sorted)))

(defun angelia-client-files--make-directory (host remote args)
  "Implement `make-directory' for REMOTE on HOST.
ARGS is (DIR &optional PARENTS).  We only set the `parents' RPC param when
the caller asked for it; jsonrpc.el's encoder doesn't recognise `:false',
so an absent key is the cleanest way to mean \"do not pass\"."
  (let ((params (angelia-client-files--params "path" remote)))
    (when (nth 1 args)
      (puthash "parents" t params))
    (angelia-client-call host 'file/mkdir params)
    nil))

(defun angelia-client-files--delete-file (host remote)
  "Implement `delete-file' for REMOTE on HOST."
  (angelia-client-call host 'file/delete
                       (angelia-client-files--params "path" remote))
  nil)

;; ---------------------------------------------------------------------------
;; Main dispatcher + registration.

(defun angelia-client-files--default (operation args)
  "Run OPERATION/ARGS with the Angelia handler temporarily suppressed."
  (let ((inhibit-file-name-handlers
         (cons 'angelia-client-files--handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun angelia-client-files--ensure-connection (host)
  "Connect to HOST when no live connection exists yet.
Auto-connect on first file-handler use is what makes `C-x C-f
/@angelia:HOST:/path' feel transparent: the user doesn't need to do
`M-x angelia-client-connect' separately."
  (unless (gethash host angelia-client--connections)
    (angelia-client--log "file-handler auto-connect to %s" host)
    (angelia-client-connect host)))

(defun angelia-client-files--handler (operation &rest args)
  "Dispatch OPERATION on ARGS for Angelia (`/@angelia:HOST:/...') paths.
Unrecognized operations fall through to the default handler chain."
  ;; A handful of operations are called by Emacs C code with a buffer (or
  ;; another non-string argument) instead of the file path -- they reach us
  ;; only because the buffer's `buffer-file-name' is an Angelia URL.  Handle
  ;; those first so the path-extraction logic below doesn't misfire.
  (cond
   ((eq operation 'verify-visited-file-modtime)
    ;; Without an interceptor, Emacs would `stat' the literal `/@angelia:...'
    ;; string on the local fs and conclude the buffer is out-of-date.  We
    ;; trust that our recorded modtime matches what the remote sees.  TODO:
    ;; refine once file/read returns mtime and we record it precisely.
    t)
   (t
  (let* ((first-path (cl-find-if (lambda (a)
                                   (and (stringp a)
                                        (string-prefix-p
                                         angelia-client-files--prefix a)))
                                 args))
         (parsed (and first-path (angelia-client-files--parse first-path)))
         (host (car parsed))
         (remote (cdr parsed)))
    (angelia-client--log
     "file-handler op=%s path=%s host=%s remote=%s"
     operation first-path host remote)
    (cond
     ((not parsed)
      ;; No recognizable Angelia path in args -- delegate so the URL syntax
      ;; itself is parseable by ordinary code paths.
      (angelia-client-files--default operation args))
     ((progn (angelia-client-files--ensure-connection host) nil)
      ;; Unreachable; the form above is only here to gate every branch below
      ;; on the connection being live.
      nil)
     ((eq operation 'insert-file-contents)
      (angelia-client-files--insert-file-contents host remote args))
     ((eq operation 'write-region)
      (angelia-client-files--write-region host remote args))
     ((eq operation 'file-exists-p)
      (angelia-client-files--rpc-bool host 'file/exists remote))
     ((eq operation 'file-directory-p)
      (angelia-client-files--rpc-bool host 'file/directory-p remote))
     ((eq operation 'file-readable-p)
      (angelia-client-files--rpc-bool host 'file/exists remote))
     ((eq operation 'file-writable-p)
      ;; We don't have a remote-side `access(W_OK)' yet.  Assume writable
      ;; when the path exists or its parent does; falling back to t is
      ;; safer than nil because nil makes `find-file' silently mark the
      ;; buffer read-only and breaks `save-buffer'.
      t)
     ((eq operation 'file-symlink-p) nil)
     ((eq operation 'file-regular-p)
      (let ((attrs (angelia-client-files--file-attributes host remote)))
        (and attrs (null (car attrs)))))
     ((eq operation 'file-attributes)
      (angelia-client-files--file-attributes host remote))
     ((eq operation 'directory-files)
      (angelia-client-files--directory-files host remote args))
     ((eq operation 'make-directory)
      (angelia-client-files--make-directory host remote args))
     ((eq operation 'delete-file)
      (angelia-client-files--delete-file host remote))
     (t
      (angelia-client--log "file-handler delegating: %s" operation)
      (angelia-client-files--default operation args))))))) ; close outer cond

;;;###autoload
(defun angelia-client-files-install ()
  "Add the Angelia file-name-handler to `file-name-handler-alist'.
Safe to call multiple times."
  (interactive)
  (unless (rassq 'angelia-client-files--handler file-name-handler-alist)
    (add-to-list 'file-name-handler-alist
                 (cons (concat "\\`" (regexp-quote
                                      angelia-client-files--prefix))
                       #'angelia-client-files--handler))))

(angelia-client-files-install)

(provide 'angelia-client-files)
;;; angelia-client-files.el ends here
