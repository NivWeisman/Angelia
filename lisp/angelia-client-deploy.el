;;; -*- lexical-binding: t; -*-
;;; angelia-client-deploy.el --- Remote bootstrap for Angelia

;; Handles the "zero-install" half of the package: SSH to a remote host,
;; verify it has Emacs >= 27, ensure ~/.cache/angelia/ exists, compare the
;; SHA1 of the cached server source against the SHA1 of our bundled copy, and
;; stream the source up if the cache is missing or stale.  Deploys are
;; idempotent -- when SHA1s match we short-circuit and return the remote
;; path immediately.
;;
;; Logging utilities live here because this is the first client-side file in
;; the build order.  `angelia-client.el' (Step 4) `require's this file, so
;; the symbols are available throughout the client surface.

(require 'cl-lib)
(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Forward declarations.

(defvar angelia-client--lisp-dir nil)
(defvar angelia-client--server-source-path nil)
(defvar angelia-client--server-source nil
  "Cached raw bytes of the local angelia-server.el file.")
(defvar angelia-client--server-sha1 nil
  "SHA1 of `angelia-client--server-source', set at load time.")

(define-error 'angelia-client-deploy-error "Angelia client deploy error")

;; ---------------------------------------------------------------------------
;; Debug logging — append-only buffer, never stderr.

(defconst angelia-client--debug-buffer-name "*angelia-client-debug*"
  "Name of the buffer that holds client-side debug output.")

(defun angelia-client--debug-buffer ()
  "Return (creating as needed) the debug buffer, with a fundamental-mode setup."
  (let ((buf (get-buffer-create angelia-client--debug-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'fundamental-mode)
        (fundamental-mode))
      (setq buffer-read-only nil
            truncate-lines t))
    buf))

(defun angelia-client--log (fmt &rest args)
  "Append a timestamped debug line built from FMT/ARGS to the debug buffer."
  (let ((line (apply #'format fmt args))
        (ts (format-time-string "%H:%M:%S.%3N")))
    (with-current-buffer (angelia-client--debug-buffer)
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s\n" ts line))))))

(defun angelia-client--log-error (where err)
  "Log a `condition-case' error ERR from context string WHERE."
  (angelia-client--log "%s ERROR %S: %s"
                       where (car err) (error-message-string err)))

(defun angelia-client--truncate (s n)
  "Return S truncated to N characters with an ellipsis when clipped."
  (if (<= (length s) n) s (concat (substring s 0 n) "…")))

;; ---------------------------------------------------------------------------
;; Local server source location + SHA1.

(setq angelia-client--lisp-dir
      (or (and load-file-name (file-name-directory load-file-name))
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))

(setq angelia-client--server-source-path
      (expand-file-name "angelia-server.el" angelia-client--lisp-dir))

(defun angelia-client-reload-server-source ()
  "Re-read the bundled angelia-server.el from disk and refresh its SHA1.
Call this after editing the server source during development; otherwise the
in-memory copy + hash diverge from the on-disk file."
  (interactive)
  (unless (file-readable-p angelia-client--server-source-path)
    (error "Cannot read server source: %s"
           angelia-client--server-source-path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally angelia-client--server-source-path)
    (setq angelia-client--server-source (buffer-string)
          angelia-client--server-sha1   (secure-hash 'sha1 (current-buffer))))
  (angelia-client--log "loaded server source: %s bytes=%d sha1=%s"
                       angelia-client--server-source-path
                       (length angelia-client--server-source)
                       angelia-client--server-sha1)
  angelia-client--server-sha1)

(angelia-client-reload-server-source)

;; ---------------------------------------------------------------------------
;; Remote paths.  Hardcoded constants -- no user input here, so we leave the
;; tilde unquoted and let the remote shell expand it.

(defconst angelia-client--remote-cache-dir "~/.cache/angelia"
  "Per-user cache directory on remote hosts where the server source lives.")

(defconst angelia-client--remote-server-path "~/.cache/angelia/server.el"
  "Path on the remote host where the deployed server source is stored.")

;; ---------------------------------------------------------------------------
;; SSH client program and POSIX quoting.

(defcustom angelia-client-ssh-program "ssh"
  "SSH client executable used for all Angelia connections.
Override with an absolute path or alternate name (e.g. \"plink\") if the
system `ssh' is not on PATH or a different client is preferred."
  :type 'string
  :group 'angelia)

(defun angelia-client--unix-quote (s)
  "Return S quoted for a POSIX remote shell, regardless of the local OS.
Uses single-quote form so metacharacters like $, &, (, ) are never expanded
before the remote bash receives the command.  Use this instead of
`shell-quote-argument' for any string destined for a Unix remote host."
  (if (string-match-p "[^a-zA-Z0-9_./-]" s)
      (concat "'" (replace-regexp-in-string "'" "'\\''" s t t) "'")
    s))

;; ---------------------------------------------------------------------------
;; Synchronous SSH plumbing.
;;
;; HOST is passed straight to `ssh' as a single argument so ~/.ssh/config
;; aliases, jump hosts, custom ports, and key files all work transparently.

(defun angelia-client--ssh-run (host args &optional stdin)
  "Run `ssh HOST bash -c CMD' synchronously, optionally feeding STDIN bytes.
ARGS is a list of strings joined into a single shell command and run under
an explicit `bash -c' invocation, so the remote login shell's identity never
matters.  Returns a plist (:exit N :stdout S :stderr S).  Captures stderr to
a temp file so we can read it back as a string."
  (angelia-client--log "ssh %s: %s%s"
                       host (string-join args " ")
                       (if stdin (format " (+%d bytes stdin)" (length stdin)) ""))
  (let* ((cmd (format "bash -c %s" (angelia-client--unix-quote (string-join args " "))))
         (stderr-file (make-temp-file "angelia-ssh-stderr-"))
         (coding-system-for-write 'binary)
         (coding-system-for-read  'binary))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((exit (call-process-region
                       (or stdin "") nil
                       angelia-client-ssh-program nil
                       (list (current-buffer) stderr-file) nil
                       host cmd)))
            (let ((stdout (buffer-string))
                  (stderr (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (when (file-exists-p stderr-file)
                              (insert-file-contents-literally stderr-file))
                            (buffer-string))))
              (angelia-client--log
               "ssh %s -> exit=%s stdout=%s stderr=%s"
               host exit
               (angelia-client--truncate stdout 200)
               (angelia-client--truncate stderr 200))
              (list :exit exit :stdout stdout :stderr stderr))))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun angelia-client--deploy-error (msg host res &rest extra)
  "Signal a deploy error with MSG, HOST, the result plist RES, and EXTRA fields."
  (signal 'angelia-client-deploy-error
          (append (list msg :host host
                        :exit (plist-get res :exit)
                        :stderr (plist-get res :stderr))
                  extra)))

;; ---------------------------------------------------------------------------
;; The deploy itself.

(defun angelia-client--remote-emacs-version (host)
  "Return the remote Emacs major version number on HOST, or signal an error.
Runs `emacs --version' and parses the first line."
  (let* ((res (angelia-client--ssh-run host '("emacs --version")))
         (out (plist-get res :stdout)))
    (unless (zerop (plist-get res :exit))
      (angelia-client--deploy-error
       "Cannot run emacs on remote host" host res :stdout out))
    (let ((ver (when (string-match "GNU Emacs \\([0-9]+\\)" out)
                 (string-to-number (match-string 1 out)))))
      (unless (and ver (>= ver 27))
        (signal 'angelia-client-deploy-error
                (list "Remote Emacs is older than 27"
                      :host host :version ver :stdout out)))
      ver)))

(defun angelia-client--remote-sha1 (host)
  "Return the SHA1 of the deployed server source on HOST, or nil if missing.
Empty/missing file produces the well-known SHA1 of empty input, which never
matches a real source -- so a `nil' return here is just a friendlier signal
to the caller that a fresh upload is required."
  (let* ((res (angelia-client--ssh-run
               host
               (list (format "cat %s 2>/dev/null | if command -v sha1sum >/dev/null 2>&1; then sha1sum; else shasum; fi"
                             angelia-client--remote-server-path))))
         (out (plist-get res :stdout)))
    (when (string-match "^\\([0-9a-f]\\{40\\}\\)" out)
      (let ((sha (match-string 1 out)))
        ;; SHA1 of empty input -- treat as "no real file there".
        (unless (equal sha "da39a3ee5e6b4b0d3255bfef95601890afd80709")
          sha)))))

(defun angelia-client--upload-server (host)
  "Stream the cached server source bytes to HOST via `cat > REMOTE-PATH'.
Signals `angelia-client-deploy-error' on a non-zero exit or post-upload
SHA1 mismatch."
  (angelia-client--log "deploy: uploading %d bytes to %s:%s"
                       (length angelia-client--server-source)
                       host angelia-client--remote-server-path)
  (let ((res (angelia-client--ssh-run
              host
              (list (format "cat > %s" angelia-client--remote-server-path))
              angelia-client--server-source)))
    (unless (zerop (plist-get res :exit))
      (angelia-client--deploy-error
       "Failed to upload server source" host res)))
  ;; Verify the bytes landed intact.
  (let ((after (angelia-client--remote-sha1 host)))
    (unless (equal after angelia-client--server-sha1)
      (signal 'angelia-client-deploy-error
              (list "Post-upload SHA1 mismatch"
                    :host host
                    :expected angelia-client--server-sha1
                    :got after)))
    (angelia-client--log "deploy: upload verified, sha1=%s" after)))

(defun angelia-client-deploy (host &optional force)
  "Ensure HOST has a current angelia-server.el deployed.
Returns the absolute remote path to the server source.  Idempotent: when
the remote SHA1 matches the embedded one this is a quick no-op probe.
When FORCE is non-nil the SHA1 check is skipped and the source is always
re-uploaded; use this to force a reload of the server after source changes.
Signals `angelia-client-deploy-error' on any failure."
  (interactive "sHost: ")
  (angelia-client--log "deploy: starting host=%s embedded-sha1=%s force=%s"
                       host angelia-client--server-sha1 (and force t))
  ;; 1. Emacs >= 27 on remote.
  (angelia-client--remote-emacs-version host)
  ;; 2. Cache directory.
  (let ((res (angelia-client--ssh-run
              host
              (list (format "mkdir -p %s" angelia-client--remote-cache-dir)))))
    (unless (zerop (plist-get res :exit))
      (angelia-client--deploy-error
       "Failed to create remote cache directory" host res)))
  ;; 3. SHA1 probe -> short-circuit if cached copy is current (skipped when forced).
  (let ((remote-sha (and (not force) (angelia-client--remote-sha1 host))))
    (angelia-client--log "deploy: remote sha1=%s embedded sha1=%s"
                         (or remote-sha (if force "<skipped>" "<missing>"))
                         angelia-client--server-sha1)
    (if (equal remote-sha angelia-client--server-sha1)
        (progn
          (angelia-client--log "deploy: sha1 match -> skip upload")
          angelia-client--remote-server-path)
      ;; 4. Upload + verify.
      (angelia-client--upload-server host)
      angelia-client--remote-server-path)))

(provide 'angelia-client-deploy)
;;; angelia-client-deploy.el ends here
