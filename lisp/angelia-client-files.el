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
(require 'angelia-client-proc)
;; `filenotify' gives us `file-notify-callback', the entry point that feeds
;; remote change events back into Emacs's watch machinery (auto-revert, etc.).
(require 'filenotify)
;; `grep' provides `grep-mode' for the `angelia-grep' results buffer.
(require 'grep)

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
  "\\`/@angelia:\\(.+?\\):\\([/~].*\\)\\'"
  "Match `/@angelia:HOST:/path' and capture HOST + remote path.
The remote path may be absolute (`/...') or home-relative (`~...', `~user/...');
the tilde is resolved on the *remote* host, never locally.")

(defun angelia-client-files--parse (path)
  "Return (HOST . REMOTE-PATH) for PATH or nil when it does not match."
  (when (and (stringp path)
             (string-match angelia-client-files--regexp path))
    (cons (match-string 1 path) (match-string 2 path))))

(defun angelia-client-files--make-path (host remote)
  "Compose an Angelia URL from HOST and REMOTE."
  (concat angelia-client-files--prefix host ":" remote))

(defun angelia-client-files--normalize-remote (remote)
  "Normalize a REMOTE path for re-wrapping into an Angelia URL.
Absolute (`/...') paths are cleaned with `expand-file-name' against `/' so
`.'/`..' collapse.  Home-relative (`~...') paths are returned untouched: the
tilde must be expanded by the *remote* host, whose $HOME may differ from ours,
so we never run them through the local `expand-file-name'."
  (if (string-prefix-p "~" remote)
      remote
    (let ((default-directory "/"))
      (expand-file-name remote "/"))))

(defun angelia-client-files--join-remote (name dir-remote)
  "Resolve relative NAME against remote directory DIR-REMOTE, tilde-preserving.
When DIR-REMOTE is home-relative (`~...'), the leading tilde segment is held out
of the local `expand-file-name' join and re-prepended, so the remote host -- not
ours -- resolves it."
  (let ((default-directory "/"))
    (if (string-prefix-p "~" dir-remote)
        (let* ((slash (string-match "/" dir-remote))
               (tilde (if slash (substring dir-remote 0 slash) dir-remote))
               (base  (if slash (substring dir-remote slash) "/")))
          (concat tilde (expand-file-name name base)))
      (expand-file-name name dir-remote))))

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

(defconst angelia-client-files--write-chunk-size (* 64 1024)
  "Bytes per chunk for chunked writes via `file/write-chunk'.")

(defconst angelia-client-files--io-timeout 120
  "Seconds before a streamed read/write gives up and signals an error.")

;; ---------------------------------------------------------------------------
;; Honest modtime + file-notify watches.

(defvar angelia-client-files--watch-counter 0
  "Monotonic id source for file-notify descriptors (stable across reconnects).")

(defvar angelia-client-files--watches (make-hash-table :test #'equal)
  "Map a file-notify descriptor (list \\='angelia-fnotify HOST ID) to a plist
(:host :remote :flags :session).  The descriptor uses a stable ID -- NOT the
session -- so it survives a reconnect: only :session is refreshed when the
watch is re-opened.  Backs `file-notify-rm-watch' / `file-notify-valid-p',
whose handler arg is the descriptor, not a path.")

;; Honest modtime.  The trick is `set-visited-file-modtime' with NO argument
;; records 0 (the \"unknown\" sentinel) for a remote path -- it cannot stat the
;; literal /@angelia:... string -- and Emacs then treats the buffer as always
;; up-to-date.  Recording the *real* remote mtime explicitly (via the existing
;; file/attributes RPC) makes the native `verify-visited-file-modtime' compare
;; correctly, so an external edit is caught instead of silently clobbered.

(defun angelia-client-files--record-visited-modtime (host remote)
  "Record REMOTE's real mtime on HOST as this buffer's visited modtime.
Falls back to `current-time' if the attributes can't be fetched, so the buffer
is never left with the always-stale 0 sentinel."
  (let ((attrs (angelia-client-files--file-attributes host remote)))
    (set-visited-file-modtime (or (and attrs (nth 5 attrs)) (current-time)))))

(defun angelia-client-files--verify-visited-modtime (buffer)
  "Belt-and-suspenders `verify-visited-file-modtime' for Angelia buffers.
The native primitive already works once we record an explicit modtime, but for
any code path that does route this op through the handler, compare the recorded
visited modtime against the file's current remote mtime: t when they match (or
nothing is recorded), nil when the remote file changed or vanished."
  (with-current-buffer (or (and (bufferp buffer) buffer) (current-buffer))
    (let ((stored (visited-file-modtime))
          (parsed (and (stringp buffer-file-name)
                       (angelia-client-files--parse buffer-file-name))))
      (cond
       ((or (null parsed) (not (consp stored)) (time-equal-p stored 0)) t)
       (t (let ((attrs (angelia-client-files--file-attributes
                        (car parsed) (cdr parsed))))
            (cond ((null attrs) nil)            ; file vanished -> changed
                  (t (time-equal-p stored (nth 5 attrs))))))))))

(defun angelia-client-files--watch-flag-strings (flags)
  "Map Emacs file-notify FLAGS to the wire strings `file/watch' expects."
  (delq nil (mapcar (lambda (f)
                      (pcase f
                        ('change "change")
                        ('attribute-change "attribute-change")))
                    (if (listp flags) flags (list flags)))))

(defun angelia-client-files--open-watch-session (desc host remote flags)
  "Open (or re-open) the `file/watch' session for DESC and record its session id.
DESC is stable, so the same closure works across reconnects.  Events feed
`file-notify-callback', which applies the single-file basename filter."
  (let* ((flag-strs (angelia-client-files--watch-flag-strings flags))
         (p (angelia-client-files--params "path" remote)))
    (puthash "flags" (vconcat flag-strs) p)
    (let ((session
           (angelia-client-open-session
            host 'file/watch p
            (lambda (kind params)
              (when (equal kind "fsevent")
                (file-notify-callback
                 (list desc
                       (intern (or (plist-get params :action) "changed"))
                       (or (plist-get params :file) "")))))
            ;; A terminal `end' means the server dropped the watch for good
            ;; (e.g. the watched dir was deleted); tell filenotify it stopped.
            ;; A mere connection drop sends no `end', so re-registration is not
            ;; pre-empted.
            :on-end
            (lambda (_params)
              (file-notify-callback (list desc 'stopped ""))))))
      (puthash desc (list :host host :remote remote :flags flags :session session)
               angelia-client-files--watches)
      session)))

(defun angelia-client-files--add-watch (host remote flags _callback)
  "Start a remote file-notify watch on directory REMOTE on HOST.
Returns a stable descriptor (list \\='angelia-fnotify HOST ID).  CALLBACK is
filenotify's concern: events are delivered through `file-notify-callback', which
looks the watch up by the descriptor and applies the single-file filter."
  (let ((desc (list 'angelia-fnotify host
                    (cl-incf angelia-client-files--watch-counter))))
    (angelia-client-files--open-watch-session desc host remote flags)
    desc))

(defun angelia-client-files--reregister-watches (host)
  "Re-open every live watch for HOST after a (re)connect, keeping descriptors.
Added to `angelia-client-after-connect-functions'; a no-op on the first connect."
  (maphash
   (lambda (desc info)
     (when (equal (plist-get info :host) host)
       (condition-case err
           (angelia-client-files--open-watch-session
            desc host (plist-get info :remote) (plist-get info :flags))
         (error (angelia-client--log-error
                 (format "reregister watch %S" desc) err)))))
   angelia-client-files--watches))

(defun angelia-client-files--rm-watch (descriptor)
  "Tear down the remote watch identified by DESCRIPTOR (the `file/unwatch' RPC)."
  (when-let ((info (gethash descriptor angelia-client-files--watches)))
    (remhash descriptor angelia-client-files--watches)
    (ignore-errors
      (angelia-client-call
       (plist-get info :host) 'file/unwatch
       (angelia-client-files--params "session" (plist-get info :session)))))
  nil)

(defun angelia-client-files--valid-watch-p (descriptor)
  "Return non-nil when DESCRIPTOR's remote watch session is still live."
  (when-let ((info (gethash descriptor angelia-client-files--watches)))
    (let ((conn (gethash (plist-get info :host) angelia-client--connections)))
      (and conn
           (gethash (plist-get info :session)
                    (angelia-client--conn-sessions conn))
           t))))

;; Re-open live watches whenever a connection (re)appears, so a dropped link
;; restores them transparently once it comes back (Step 3).
(add-hook 'angelia-client-after-connect-functions
          #'angelia-client-files--reregister-watches)

;; ---------------------------------------------------------------------------
;; Remote search (Step 4).  One `file/search' session runs rg/grep on the remote
;; host and streams hits, instead of fanning out a process-file per file (the
;; pattern the TRAMP comparison punished).

(defcustom angelia-client-files-search-max 2000
  "Cap on matches a single `angelia-grep' / `file/search' returns (0 = no cap)."
  :type 'integer
  :group 'angelia)

(defun angelia-client-files--search-params (remote regexp max)
  "Build the `file/search' params hash for REMOTE dir, REGEXP, and MAX cap."
  (let ((p (angelia-client-files--params "path" remote "regexp" regexp)))
    (when (and (integerp max) (> max 0)) (puthash "max" max p))
    p))

(defun angelia-client-files-search (host remote regexp &optional max)
  "Search the REMOTE directory on HOST for REGEXP; block until done.
Returns a list of (FILE LINE COL TEXT), FILE an Angelia URL.  MAX caps the
result count (default `angelia-client-files-search-max')."
  (let ((matches '())
        (ended nil)
        (cap (or max angelia-client-files-search-max)))
    (angelia-client-open-session
     host 'file/search
     (angelia-client-files--search-params remote regexp cap)
     (lambda (kind params)
       (when (equal kind "match")
         (push (list (angelia-client-files--make-path host (plist-get params :file))
                     (plist-get params :line)
                     (plist-get params :col)
                     (plist-get params :text))
               matches)))
     :on-end (lambda (_p) (setq ended t)))
    (with-timeout (angelia-client-files--io-timeout
                   (error "file/search timed out for %s" remote))
      (while (not ended) (accept-process-output nil 0.05)))
    (nreverse matches)))

(defun angelia-grep (dir regexp &optional max)
  "Search the Angelia directory DIR for REGEXP, streaming hits to a grep buffer.
RET on a line jumps to the match (through the Angelia file handler).  DIR
defaults to `default-directory' when it is already an Angelia path."
  (interactive
   (let* ((d (if (and (stringp default-directory)
                      (angelia-client-files--parse default-directory))
                 default-directory
               (read-string "Angelia search dir (/@angelia:HOST:/path): ")))
          (r (read-string "Search regexp: ")))
     (list d r angelia-client-files-search-max)))
  (let* ((parsed (or (angelia-client-files--parse dir)
                     (error "angelia-grep: not an Angelia directory: %s" dir)))
         (host (car parsed))
         (remote (cdr parsed))
         (buf (get-buffer-create "*angelia-grep*")))
    (angelia-client-files--ensure-connection host)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: grep -*-\n\nAngelia search: %s  in  %s\n\n"
                        regexp dir)))
      (grep-mode)
      (setq-local default-directory dir))
    (display-buffer buf)
    (angelia-client-open-session
     host 'file/search
     (angelia-client-files--search-params remote regexp
                                          (or max angelia-client-files-search-max))
     (lambda (kind params)
       (when (and (equal kind "match") (buffer-live-p buf))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert (format "%s:%s:%s:%s\n"
                               (angelia-client-files--make-path
                                host (plist-get params :file))
                               (plist-get params :line)
                               (plist-get params :col)
                               (plist-get params :text))))))))
     :on-end (lambda (_p)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (save-excursion (goto-char (point-max))
                                     (insert "\nSearch finished\n")))))))
    buf))

;; ---------------------------------------------------------------------------
;; File-handler completeness + remote edit locking (Step 5).

(defcustom angelia-client-files-enable-locks nil
  "When non-nil, take real remote edit locks on Angelia buffers.
Off by default: Angelia buffers keep `create-lockfiles' nil (no lock files),
preserving today's behaviour.  Enabling it makes editing a remote buffer create
an Emacs-style `.#NAME' lock on the remote host, so a second client detects the
in-progress edit via `file-locked-p'.  `lock-file' / `unlock-file' /
`file-locked-p' work regardless of this flag; it only controls auto-locking."
  :type 'boolean
  :group 'angelia)

(defun angelia-client-files--lock-owner ()
  "Return this Emacs's lock-owner string, Emacs's `user@host.pid' convention."
  (format "%s@%s.%d" (user-login-name) (system-name) (emacs-pid)))

(defun angelia-client-files--file-newer-than (host remote other)
  "Implement `file-newer-than-file-p': is REMOTE on HOST newer than OTHER?
OTHER is the second argument (any file name).  t when REMOTE exists and OTHER
is older or absent; nil when REMOTE is absent."
  (let* ((a (angelia-client-files--file-attributes host remote))
         (other-parsed (angelia-client-files--parse other))
         (b (and other-parsed
                 (angelia-client-files--file-attributes
                  (car other-parsed) (cdr other-parsed)))))
    (cond ((null a) nil)
          ((null b) t)
          (t (time-less-p (nth 5 b) (nth 5 a))))))

(defun angelia-client-files--system-info (host remote)
  "Implement `file-system-info': return (TOTAL FREE AVAIL) bytes or nil."
  (let* ((resp (angelia-client-call host 'file/fs-info
                                    (angelia-client-files--params "path" remote)))
         (total (plist-get resp :total)))
    (when total
      (list total (plist-get resp :free) (plist-get resp :avail)))))

(defun angelia-client-files--locked-p (host remote)
  "Implement `file-locked-p': nil (unlocked), t (locked by us), or owner string."
  (let* ((resp (angelia-client-call host 'file/locked-p
                                    (angelia-client-files--params "path" remote)))
         (owner (plist-get resp :owner)))
    (cond ((or (null owner) (eq owner :null)) nil)
          ((equal owner (angelia-client-files--lock-owner)) t)
          (t owner))))

(defun angelia-client-files--insert-file-contents (host remote args)
  "Read REMOTE on HOST via chunked `file/read' and insert the bytes here.
ARGS is the full argument list to `insert-file-contents'.  Chunks arrive
as `session/event' notifications; we accumulate them under a registered
callback then insert the joined bytes (decoded as UTF-8) at point."
  (let* ((filename (nth 0 args))
         (visit    (nth 1 args))
         (beg      (nth 2 args))
         (end      (nth 3 args))
         (replace  (nth 4 args))
         (chunks   '())
         (ended    nil)
         (failed   nil))
    (angelia-client-open-session
     host 'file/read
     (angelia-client-files--params "path" remote)
     (lambda (kind params)
       (pcase kind
         ("chunk"
          (push (base64-decode-string (plist-get params :data)) chunks))
         ("error"
          (setq failed (or (plist-get params :message) "remote read error")))))
     :on-end (lambda (_p) (setq ended t)))
    (with-timeout (angelia-client-files--io-timeout
                   (error "file/read timed out for %s" remote))
      (while (not ended)
        (accept-process-output nil 0.05)))
    (when failed (error "file/read: %s" failed))
    (let* ((all (apply #'concat (nreverse chunks)))
           (selected (cond ((and beg end) (substring all beg end))
                           (beg           (substring all beg))
                           (end           (substring all 0 end))
                           (t             all))))
      (when replace
        (delete-region (point-min) (point-max)))
      ;; Buffer is multibyte by default; binary content still survives the
      ;; base64 transport but the in-buffer view may be munged.
      (let ((decoded (decode-coding-string selected 'utf-8 t)))
        (insert decoded)
        ;; The contract returns CHARACTERS inserted, not bytes.
        (setq selected decoded))
      (when visit
        (setq buffer-file-name filename)
        (set-buffer-modified-p nil)
        ;; Record the remote file's mtime as the buffer's last-visited modtime.
        ;; With no arg, `set-visited-file-modtime' looks up `file-attributes'
        ;; on the visited path -- which routes through our handler -- so the
        ;; Record the *real* remote mtime (not the no-arg 0 sentinel) so a
        ;; later external edit makes `verify-visited-file-modtime' return nil
        ;; and the user is warned instead of silently clobbering the change.
        (angelia-client-files--record-visited-modtime host remote)
        ;; Disable on-save backups + autosave for this buffer: each operates on
        ;; the local fs path, which doesn't exist, so `save-buffer' would fail
        ;; with `file-missing'.  Lock files, by contrast, now have working
        ;; remote handlers, so honour `angelia-client-files-enable-locks'
        ;; (default nil keeps today's no-lock behaviour).
        (setq-local backup-inhibited t)
        (setq-local create-lockfiles angelia-client-files-enable-locks)
        (auto-save-mode -1))
      (list filename (length selected)))))

(defun angelia-client-files--write-region (host remote args)
  "Stream the START..END region of the current buffer to REMOTE on HOST.
ARGS is the full argument list to `write-region'.  Uses the three-phase
`file/write-open' -> `file/write-chunk' (repeated) -> `file/write-finish'
flow.  On any error mid-stream, sends `session/close' so the server drops
its in-progress tmp file."
  (let* ((start (nth 0 args))
         (end   (nth 1 args))
         (filename (nth 2 args))
         (append (nth 3 args))
         (visit (nth 4 args))
         (mustbenew (nth 6 args))
         (bytes (cond
                 ((stringp start) start)
                 ;; `write-region' allows START=END=nil to mean "entire
                 ;; accessible buffer", which is what `save-buffer' passes.
                 ((and (null start) (null end))
                  (buffer-substring-no-properties (point-min) (point-max)))
                 (t (buffer-substring-no-properties start end))))
         (encoded (encode-coding-string bytes 'utf-8 t))
         (total (length encoded))
         (chunk-size angelia-client-files--write-chunk-size))
    (when append
      (error "angelia: write-region :append is not implemented"))
    ;; MUSTBENEW: `excl' must signal; any other non-nil asks the user, like
    ;; the local primitive.  Probe-then-write is not atomic (the remote
    ;; rename can still race another writer), but it honours the contract
    ;; instead of silently clobbering.
    (when (and mustbenew
               (angelia-client-files--rpc-bool host 'file/exists remote))
      (if (or (eq mustbenew 'excl)
              (not (y-or-n-p (format "File %s exists; overwrite? " filename))))
          (signal 'file-already-exists (list "File already exists" filename))))
    ;; Open via `angelia-client-open-session' so the local session row is
    ;; registered (with no-op callbacks) before any events arrive.  The
    ;; server emits a `kind: "end"' notification from `--end-session' inside
    ;; `file/write-finish'; our no-op `on-end' simply unregisters the row.
    (let ((session (angelia-client-open-session
                    host 'file/write-open
                    (angelia-client-files--params
                     "path" remote
                     "size" total)
                    (lambda (_kind _params) nil)
                    :on-end (lambda (_p) nil)
                    :timeout angelia-client-files--io-timeout)))
      (condition-case err
          (progn
            (let ((offset 0))
              (while (< offset total)
                (let* ((chunk-end (min total (+ offset chunk-size)))
                       (chunk (substring encoded offset chunk-end))
                       (p (make-hash-table :test #'equal)))
                  (puthash "session" session p)
                  (puthash "data" (base64-encode-string chunk t) p)
                  (angelia-client-call host 'file/write-chunk p
                                       angelia-client-files--io-timeout)
                  (setq offset chunk-end))))
            (let ((p (make-hash-table :test #'equal)))
              (puthash "session" session p)
              (angelia-client-call host 'file/write-finish p
                                   angelia-client-files--io-timeout)))
        (error
         (angelia-client-close-session host session)
         (signal (car err) (cdr err)))))
    (when (or (eq visit t) (stringp visit))
      (setq buffer-file-name (if (stringp visit) visit filename))
      (set-buffer-modified-p nil)
      ;; Record the just-written file's real mtime so the next
      ;; `verify-visited-file-modtime' does not mistake our own save for an
      ;; external edit (a no-arg `set-visited-file-modtime' would record 0).
      (angelia-client-files--record-visited-modtime host remote))
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

(defun angelia-client-files--take (list count)
  "Return the first COUNT elements of LIST, or LIST when COUNT is not a count.
Backs the COUNT argument of `directory-files' (and -and-attributes)."
  (if (and (integerp count) (>= count 0) (< count (length list)))
      (cl-subseq list 0 count)
    list))

(defun angelia-client-files--directory-files (host remote args)
  "Implement `directory-files' for REMOTE on HOST.
ARGS is the full argument list (DIRECTORY FULL MATCH NOSORT &optional COUNT)."
  (let* ((directory (nth 0 args))
         (full      (nth 1 args))
         (match     (nth 2 args))
         (nosort    (nth 3 args))
         (count     (nth 4 args))
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
         (sorted (angelia-client-files--take
                  (if nosort filtered (sort filtered #'string<)) count))
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

(defun angelia-client-files--delete-directory (host remote recursive)
  "Implement `delete-directory' for REMOTE on HOST."
  (let ((p (angelia-client-files--params "path" remote)))
    (when recursive (puthash "recursive" t p))
    (angelia-client-call host 'file/delete-directory p)
    nil))

(defun angelia-client-files--two-path-op (method src-host src-rem dst args ok-idx)
  "Run METHOD with `from'=SRC-REM and `to'=remote portion of DST on SRC-HOST.
Both paths must live on the same host.  ARGS is the original argument list;
OK-IDX is the index of the OK-IF-EXISTS argument in ARGS (e.g. 2 for
`copy-file', 2 for `rename-file').  Returns nil."
  (let* ((dst-parsed (angelia-client-files--parse dst))
         (dst-host (car dst-parsed))
         (dst-rem  (cdr dst-parsed))
         (ok (nth ok-idx args)))
    (unless dst-parsed
      (error "angelia: cross-realm %s to non-angelia path not supported: %s"
             method dst))
    (unless (equal dst-host src-host)
      (error "angelia: cross-host %s not supported (%s -> %s)"
             method src-host dst-host))
    (let ((p (make-hash-table :test #'equal)))
      (puthash "from" src-rem p)
      (puthash "to"   dst-rem p)
      (when ok (puthash "ok-if-exists" t p))
      (angelia-client-call src-host method p)
      nil)))

(defun angelia-client-files--directory-files-and-attributes (host remote args)
  "Implement `directory-files-and-attributes' for REMOTE on HOST.
ARGS is (DIRECTORY &optional FULL MATCH NOSORT ID-FORMAT COUNT)."
  (let* ((directory (nth 0 args))
         (full      (nth 1 args))
         (match     (nth 2 args))
         (nosort    (nth 3 args))
         (count     (nth 5 args))
         (resp (angelia-client-call
                host 'file/list-dir-attrs
                (angelia-client-files--params "path" remote)))
         (entries (append (plist-get resp :entries) nil))
         (filtered (if match
                       (cl-remove-if-not
                        (lambda (e) (string-match-p match (plist-get e :name)))
                        entries)
                     entries))
         (sorted (angelia-client-files--take
                  (if nosort filtered
                    (sort filtered
                          (lambda (a b)
                            (string< (plist-get a :name)
                                     (plist-get b :name)))))
                  count))
         (dir (if (string-suffix-p "/" directory)
                  directory (concat directory "/"))))
    (mapcar
     (lambda (e)
       (let* ((name (plist-get e :name))
              (type (plist-get e :type))
              (size (plist-get e :size))
              (mode (plist-get e :mode))
              (mtime-str (plist-get e :mtime))
              (mtime (if mtime-str
                         (ignore-errors (date-to-time mtime-str))
                       '(0 0 0 0)))
              (type-val (cond ((equal type "directory") t)
                              ((equal type "symlink") "")
                              (t nil)))
              (path (if full (concat dir name) name)))
         (cons path
               (list type-val 1 0 0 mtime mtime mtime size mode nil 0 0))))
     sorted)))

(defun angelia-client-files--completions (host remote file)
  "Return the list of completion candidates for FILE under REMOTE on HOST."
  (let* ((p (angelia-client-files--params "path" remote "file" (or file ""))))
    (let* ((resp (angelia-client-call host 'file/completions p))
           (names (plist-get resp :names)))
      (append names nil))))

(defun angelia-client-files--mode-string-to-number (modes)
  "Convert an ls-style MODES string (\"drwxr-xr-x\") to an integer, or nil.
`file-modes-symbolic-to-number' canNOT do this -- it parses \"u+x\"-style
specs and signals a parse error on ls output -- so walk the nine
permission characters by hand (setuid/setgid/sticky included)."
  (when (and (stringp modes) (= (length modes) 10))
    (let ((bits 0))
      (cl-loop for i from 1 to 9
               for ch = (aref modes i)
               for bit = (ash 1 (- 9 i))
               do (pcase ch
                    ((or ?r ?w ?x) (setq bits (logior bits bit)))
                    ;; Lower-case s/t carry the execute bit too.
                    ((or ?s ?t) (setq bits (logior bits bit)))))
      (when (memq (aref modes 3) '(?s ?S)) (setq bits (logior bits #o4000)))
      (when (memq (aref modes 6) '(?s ?S)) (setq bits (logior bits #o2000)))
      (when (memq (aref modes 9) '(?t ?T)) (setq bits (logior bits #o1000)))
      bits)))

(defun angelia-client-files--file-modes (host remote)
  "Implement `file-modes' for REMOTE on HOST -- returns an integer or nil."
  (let ((attrs (angelia-client-files--file-attributes host remote)))
    (when attrs
      (angelia-client-files--mode-string-to-number (nth 8 attrs)))))

(defun angelia-client-files--insert-directory (host remote args)
  "Implement `insert-directory' by shelling out to ls on the remote.
ARGS is the original (FILE SWITCHES &optional WILDCARD FULL-DIRECTORY-P)."
  (let* ((_file (nth 0 args))
         (switches (nth 1 args))
         (_wildcard (nth 2 args))
         (sw-list (cond ((listp switches) switches)
                        ((stringp switches)
                         (split-string-and-unquote switches))
                        (t nil)))
         (argv (append (list "ls") sw-list (list remote)))
         (result (angelia-client--exec host argv)))
    (insert (decode-coding-string (plist-get result :stdout) 'utf-8 t))
    (let ((stderr (plist-get result :stderr)))
      (when (and stderr (> (length stderr) 0))
        (angelia-client--log "insert-directory stderr: %s"
                             (angelia-client--truncate stderr 200))))))

(defun angelia-client-files--resolve-bufferspec (buffer)
  "Translate process-file's BUFFER spec into (STDOUT-BUF . STDERR-DEST).
STDERR-DEST is nil (discard / default mixing), the symbol `mix' (mix into
STDOUT-BUF, the meaning of t in a cons spec), a file name string, or a
buffer (used by the `shell-command' handler, whose ERROR-BUFFER arg is a
buffer or buffer name rather than a file)."
  (cond
   ((null buffer)              (cons nil nil))
   ((eq buffer t)              (cons (current-buffer) 'mix))
   ((eq buffer 0)              (cons nil nil))
   ((bufferp buffer)           (cons buffer 'mix))
   ((stringp buffer)           (cons (get-buffer-create buffer) 'mix))
   ((consp buffer)
    (let* ((sb (car buffer))
           (eb (cadr buffer))
           (so (cond ((null sb) nil)
                     ((eq sb t) (current-buffer))
                     ((bufferp sb) sb)
                     ((stringp sb) (get-buffer-create sb))))
           (se (cond ((null eb) nil)
                     ((eq eb t) 'mix)
                     ((bufferp eb) eb)
                     ((stringp eb) eb))))
      (cons so se)))
   (t (cons (current-buffer) 'mix))))

(defun angelia-client-files--process-file (host remote-dir args)
  "Implement `process-file' for an angelia `default-directory'.
ARGS is (PROGRAM &optional INFILE BUFFER DISPLAY &rest CMDARGS)."
  (let* ((program (nth 0 args))
         (infile  (nth 1 args))
         (buffer  (nth 2 args))
         (_display (nth 3 args))
         (cmdargs (nthcdr 4 args))
         (stdin (cond
                 ((null infile) nil)
                 ((stringp infile)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally infile)
                    (buffer-string)))
                 ((consp infile)
                  ;; (FILE) form means same as FILE; (FILE BEG END) is a buffer
                  ;; range -- skip for now.
                  (let ((f (car infile)))
                    (when (stringp f)
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally f)
                        (buffer-string)))))
                 (t nil)))
         (dest (angelia-client-files--resolve-bufferspec buffer))
         (out-buf (car dest))
         (err-dest (cdr dest))
         (result (angelia-client--exec host (cons program cmdargs)
                                       :cwd remote-dir
                                       :stdin stdin))
         (exit (plist-get result :exit))
         (signal (plist-get result :signal))
         (stdout (plist-get result :stdout))
         (stderr (plist-get result :stderr)))
    (when (and out-buf (> (length stdout) 0))
      (with-current-buffer out-buf
        (insert (decode-coding-string stdout 'utf-8 t))))
    (when (> (length stderr) 0)
      (cond
       ((stringp err-dest)
        (let ((coding-system-for-write 'binary))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert stderr)
            (write-region (point-min) (point-max) err-dest nil 'silent))))
       ((bufferp err-dest)
        (with-current-buffer err-dest
          (insert (decode-coding-string stderr 'utf-8 t))))
       ((and (eq err-dest 'mix) out-buf)
        (with-current-buffer out-buf
          (insert (decode-coding-string stderr 'utf-8 t))))))
    (cond ((and signal (not (eq signal :null)))
           (format "signal: %s" signal))
          ((integerp exit) exit)
          (t -1))))

(defun angelia-client-files--start-file-process (host remote-dir args)
  "Implement `start-file-process' for an angelia `default-directory'.
ARGS is (NAME BUFFER PROGRAM &rest CMDARGS).  Returns a process object."
  (let* ((name    (nth 0 args))
         (buffer  (nth 1 args))
         (program (nth 2 args))
         (cmdargs (nthcdr 3 args))
         (buf (cond ((null buffer) nil)
                    ((bufferp buffer) buffer)
                    ((stringp buffer) (get-buffer-create buffer))
                    (t nil))))
    (angelia-client--exec-async host (cons program cmdargs) buf name
                                :cwd remote-dir)))

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
    ;; string on the local fs and conclude the buffer is out-of-date.  Compare
    ;; the recorded remote mtime against the file's current remote mtime so an
    ;; external edit is actually detected (not silently clobbered).
    (angelia-client-files--verify-visited-modtime (car args)))
   ;; `file-notify-rm-watch' / `file-notify-valid-p' are dispatched with the
   ;; opaque DESCRIPTOR (not a path), so they must be handled before the
   ;; path-extraction below -- which would otherwise find no angelia string and
   ;; delegate.  `file-notify-add-watch' carries the directory path and is
   ;; handled in the main cond.
   ((eq operation 'file-notify-rm-watch)
    (angelia-client-files--rm-watch (nth 0 args)))
   ((eq operation 'file-notify-valid-p)
    (angelia-client-files--valid-watch-p (nth 0 args)))
   (t
  (let* ((first-path (cl-find-if (lambda (a)
                                   (and (stringp a)
                                        (string-prefix-p
                                         angelia-client-files--prefix a)))
                                 args))
         (parsed (and first-path (angelia-client-files--parse first-path)))
         ;; A small set of operations carry no angelia path in `args' --
         ;; they reach us only because `default-directory' is an angelia
         ;; URL.  For those (and ONLY those) we resolve the connection via
         ;; `default-directory'.  Path-arg ops must NOT fall through to
         ;; `dd-parsed', or a probe like `(file-directory-p "/@angelia:H:")'
         ;; (no path component) would send a nil `path' to the server.
         (dd-op (memq operation
                      '(process-file start-file-process shell-command
                        temporary-file-directory
                        unhandled-file-name-directory
                        make-nearby-temp-file file-remote-p)))
         (dd-parsed (and dd-op (not parsed)
                         (stringp default-directory)
                         (angelia-client-files--parse default-directory)))
         (effective (or parsed dd-parsed))
         (host (car effective))
         (remote (cdr parsed))
         (remote-dir (or remote (cdr dd-parsed))))
    (angelia-client--log
     "file-handler op=%s path=%s host=%s remote=%s dd=%s"
     operation first-path host remote default-directory)
    (cond
     ((not effective)
      ;; No recognizable Angelia path here -- delegate so the URL syntax
      ;; itself is parseable by ordinary code paths.
      (angelia-client-files--default operation args))
     ((and (not (memq operation
                      '(process-file start-file-process shell-command
                        temporary-file-directory
                        unhandled-file-name-directory
                        make-nearby-temp-file file-remote-p
                        expand-file-name)))
           (null remote))
      ;; The path arg parsed only as far as `/@angelia:HOST:' with no
      ;; remote component -- nothing useful to send.  Treat like
      ;; \"directory above the root\" and delegate.
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
      (angelia-client-files--rpc-bool host 'file/writable-p remote))
     ((eq operation 'file-executable-p)
      (angelia-client-files--rpc-bool host 'file/executable-p remote))
     ((eq operation 'file-symlink-p)
      (let ((resp (condition-case nil
                      (angelia-client-call
                       host 'file/symlink-target
                       (angelia-client-files--params "path" remote))
                    (error :null))))
        (and (stringp resp) resp)))
     ((eq operation 'file-regular-p)
      (let ((attrs (angelia-client-files--file-attributes host remote)))
        (and attrs (null (car attrs)))))
     ((eq operation 'file-equal-p)
      ;; Two files are equal iff same host + same normalized remote path.  The
      ;; default delegation wrongly returns t for any two angelia paths (it
      ;; stats the literal URLs locally), so we must answer here.
      (let ((other (angelia-client-files--parse (nth 1 args))))
        (and other (equal host (car other))
             (equal (angelia-client-files--normalize-remote remote)
                    (angelia-client-files--normalize-remote (cdr other))))))
     ((eq operation 'file-newer-than-file-p)
      (angelia-client-files--file-newer-than host remote (nth 1 args)))
     ((eq operation 'access-file)
      ;; Signal like the local primitive when the file is not accessible.
      (unless (angelia-client-files--rpc-bool host 'file/exists remote)
        (signal 'file-missing (list (or (nth 1 args) "Opening input file")
                                    (nth 0 args))))
      nil)
     ((eq operation 'file-system-info)
      (angelia-client-files--system-info host remote))
     ((eq operation 'lock-file)
      (angelia-client-call host 'file/lock
                           (angelia-client-files--params
                            "path" remote
                            "owner" (angelia-client-files--lock-owner)))
      nil)
     ((eq operation 'unlock-file)
      (angelia-client-call host 'file/unlock
                           (angelia-client-files--params
                            "path" remote
                            "owner" (angelia-client-files--lock-owner)))
      nil)
     ((eq operation 'file-locked-p)
      (angelia-client-files--locked-p host remote))
     ((eq operation 'file-attributes)
      (angelia-client-files--file-attributes host remote))
     ((eq operation 'file-modes)
      (angelia-client-files--file-modes host remote))
     ((eq operation 'set-file-modes)
      (let ((mode (nth 1 args))
            (p (angelia-client-files--params "path" remote)))
        (when (integerp mode) (puthash "mode" mode p))
        (angelia-client-call host 'file/set-modes p)
        nil))
     ((eq operation 'set-file-times)
      (let* ((time (nth 1 args))
             (epoch (and time (float-time time)))
             (p (angelia-client-files--params "path" remote)))
        (when (numberp epoch) (puthash "time" epoch p))
        (angelia-client-call host 'file/set-times p)
        nil))
     ((eq operation 'directory-files)
      (angelia-client-files--directory-files host remote args))
     ((eq operation 'directory-files-and-attributes)
      (angelia-client-files--directory-files-and-attributes host remote args))
     ((eq operation 'file-name-all-completions)
      (let ((file (nth 0 args)))
        (angelia-client-files--completions host remote file)))
     ((eq operation 'file-name-completion)
      (let* ((file (nth 0 args))
             (pred (nth 2 args))
             (cands (angelia-client-files--completions host remote file)))
        (try-completion file (mapcar #'list cands) pred)))
     ((eq operation 'make-directory)
      (angelia-client-files--make-directory host remote args))
     ((eq operation 'delete-file)
      (angelia-client-files--delete-file host remote))
     ((eq operation 'delete-directory)
      (angelia-client-files--delete-directory host remote (nth 1 args)))
     ((eq operation 'copy-file)
      (angelia-client-files--two-path-op
       'file/copy host remote (nth 1 args) args 2))
     ((eq operation 'rename-file)
      (angelia-client-files--two-path-op
       'file/rename host remote (nth 1 args) args 2))
     ((eq operation 'make-symbolic-link)
      (let* ((target (nth 0 args))
             (link (nth 1 args))
             (ok (nth 2 args))
             (link-parsed (angelia-client-files--parse link))
             (p (make-hash-table :test #'equal)))
        (unless link-parsed
          (error "angelia: link path must be an angelia URL"))
        (puthash "target" target p)
        (puthash "linkpath" (cdr link-parsed) p)
        (when ok (puthash "ok-if-exists" t p))
        (angelia-client-call (car link-parsed) 'file/symlink p)
        nil))
     ((eq operation 'insert-directory)
      (angelia-client-files--insert-directory host remote args))
     ((eq operation 'file-notify-add-watch)
      ;; ARGS is (DIR FLAGS CALLBACK): filenotify already reduced a file watch
      ;; to its parent directory, which is what `remote' is here.
      (angelia-client-files--add-watch host remote (nth 1 args) (nth 2 args)))
     ((eq operation 'process-file)
      (angelia-client-files--process-file host remote-dir args))
     ((eq operation 'start-file-process)
      (angelia-client-files--start-file-process host remote-dir args))
     ((eq operation 'shell-command)
      ;; Route shell-command via process-file using the user's `shell-file-name'
      ;; on the remote.  Magit and many other packages rely on this.
      (let* ((cmd (nth 0 args))
             (out (nth 1 args))
             (err (nth 2 args))
             ;; ERROR-BUFFER is a buffer or buffer name; hand the buffer
             ;; object to the cons spec so `--resolve-bufferspec' routes
             ;; stderr there instead of dropping it.
             (buffer-spec (if err
                              (list (or out t) (get-buffer-create err))
                            (or out t))))
        (angelia-client-files--process-file
         host remote-dir
         (list shell-file-name nil buffer-spec nil
               shell-command-switch cmd))))
     ((eq operation 'file-remote-p)
      ;; Return the prefix that uniquely identifies the connection: magit
      ;; uses this as a key to decide "is this remote at all and which one".
      (concat angelia-client-files--prefix host ":"))
     ((eq operation 'unhandled-file-name-directory)
      ;; Some local routines need a chdir-able directory (dired does this
      ;; for its buffer's `default-directory').  We don't have a remote
      ;; chdir, but pointing at the local /tmp is what TRAMP does and works
      ;; in practice -- the buffer's logical default-directory remains
      ;; angelia, only the OS-level cwd of the process tree falls back.
      temporary-file-directory)
     ((eq operation 'temporary-file-directory)
      (angelia-client-files--make-path host "/tmp/"))
     ((eq operation 'make-nearby-temp-file)
      ;; (make-nearby-temp-file PREFIX &optional DIR-FLAG SUFFIX) -- create
      ;; a temp file living on the same host as the surrounding code so
      ;; subsequent ops (chmod, rename) don't cross the boundary.
      (let* ((prefix (nth 0 args))
             (dir-flag (nth 1 args))
             (suffix (nth 2 args))
             (argv (append
                    (list "mktemp")
                    (when dir-flag (list "-d"))
                    (list (concat "/tmp/" prefix "XXXXXX"
                                  (or suffix "")))))
             (result (angelia-client--exec host argv))
             (name (string-trim (plist-get result :stdout))))
        (angelia-client-files--make-path host name)))
     ((eq operation 'expand-file-name)
      ;; (expand-file-name NAME &optional DIR).  Shapes we handle:
      ;;   (a) NAME is an angelia URL                -> clean up its remote part
      ;;   (b) NAME is relative, DIR is angelia URL  -> resolve NAME against DIR
      ;;       and re-wrap with the same host
      ;;   (c) NAME is absolute non-angelia          -> delegate; absolute
      ;;       local paths must stay local even when DIR is angelia, otherwise
      ;;       Emacs's loader (autoload of `dired-aux' etc.) tries to read
      ;;       `/@angelia:HOST:/usr/share/emacs/...' and signals file-missing.
      ;;   (d) nothing angelia in either argument     -> delegate
      (let* ((name (nth 0 args))
             (dir  (or (nth 1 args) default-directory))
             (name-parsed (and (stringp name)
                               (angelia-client-files--parse name)))
             (name-absolute-p (and (stringp name)
                                   (> (length name) 0)
                                   (eq (aref name 0) ?/)))
             (dir-parsed  (and (not name-parsed)
                               (not name-absolute-p)
                               (stringp dir)
                               (angelia-client-files--parse dir))))
        (cond
         (name-parsed
          (angelia-client-files--make-path
           (car name-parsed)
           (angelia-client-files--normalize-remote (cdr name-parsed))))
         ;; `~...' relative to an angelia DIR is home-relative on the
         ;; REMOTE host: wrap it verbatim (the remote expands the tilde).
         ;; Joining it would run it through the local `expand-file-name',
         ;; which substitutes the LOCAL home directory.
         ((and dir-parsed (string-prefix-p "~" name))
          (angelia-client-files--make-path (car dir-parsed) name))
         (dir-parsed
          (angelia-client-files--make-path
           (car dir-parsed)
           (angelia-client-files--join-remote name (cdr dir-parsed))))
         (t
          (angelia-client-files--default operation args)))))
     ((eq operation 'file-truename)
      ;; Remote symlinks are not followed yet (no RPC for that).  Returning
      ;; the path unchanged is safe; find-file will not second-guess it.
      first-path)
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
