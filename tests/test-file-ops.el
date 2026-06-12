;;; -*- lexical-binding: t; -*-
;;; test-file-ops.el --- Layer 2 file-operation tests (via /@angelia: handler)

;; Exercises the eight file operations the Angelia file-name-handler
;; implements end-to-end (client -> ssh -> remote server -> filesystem).
;; Requires passwordless `ssh localhost' configured.
;;
;; A note on dired: `dired-noselect' needs `chdir(2)' on the buffer's
;; `default-directory', which is something TRAMP handles via additional
;; ops (`unhandled-file-name-directory', etc.) not in our scope.  The
;; `test-dired-remote' test below verifies the underlying primitive
;; (`directory-files') that dired uses to render listings.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)

(defconst angelia-tests--target-host "localhost"
  "Single-VM test target.")

(defun angelia-tests--remote (local-path)
  "Convert LOCAL-PATH to its `/@angelia:HOST:LOCAL-PATH' form."
  (concat "/@angelia:" angelia-tests--target-host ":" local-path))

(defmacro angelia-tests--with-remote-temp-file (var content &rest body)
  "Bind VAR to a fresh temp file containing CONTENT, run BODY, cleanup."
  (declare (indent 2))
  `(let ((,var (make-temp-file "angelia-test-file-")))
     (unwind-protect
         (progn
           (angelia-tests-write-file ,var ,content)
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun angelia-tests--file-ops-setup ()
  (angelia-tests-ensure-no-connections))

;; ---------------------------------------------------------------------------

(ert-deftest test-find-file-remote ()
  "Opening /@angelia:HOST:/etc/hostname yields a buffer with the file's bytes."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((path (angelia-tests--remote "/etc/hostname"))
           (buf (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buf
            (should (equal buffer-file-name path))
            (should (> (buffer-size) 0)))
        (kill-buffer buf)))))

(ert-deftest test-find-file-remote-tilde ()
  "Opening /@angelia:HOST:~/FILE reads the full file via remote tilde expansion.
Regression: the URL regexp once required a leading `/' in the remote path, so a
`~'-relative URL silently fell through to the local filesystem and produced an
empty buffer.  The tilde must be resolved on the remote host."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    ;; localhost: remote $HOME == ours, so we can place a real file under ~.
    (let* ((name (format ".angelia-tilde-test-%d" (emacs-pid)))
           (abs  (expand-file-name name "~/"))
           (body "line-one\nline-two\nline-three\n"))
      (unwind-protect
          (progn
            (angelia-tests-write-file abs body)
            (let ((buf (find-file-noselect
                        (concat "/@angelia:" angelia-tests--target-host ":~/" name))))
              (unwind-protect
                  (with-current-buffer buf
                    (should (equal (buffer-string) body))
                    (should (= 3 (count-lines (point-min) (point-max)))))
                (kill-buffer buf))))
        (when (file-exists-p abs) (delete-file abs))))))

(ert-deftest test-save-file-remote ()
  "Open a remote temp file, modify it, save, re-read, verify the new content."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (angelia-tests--with-remote-temp-file local "initial\n"
      (let* ((remote (angelia-tests--remote local))
             (buf (find-file-noselect remote)))
        (unwind-protect
            (with-current-buffer buf
              (should (equal (buffer-string) "initial\n"))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "updated content\n"))
              (save-buffer)
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally local)
                (should (equal (buffer-string) "updated content\n"))))
          (kill-buffer buf))))))

(ert-deftest test-file-exists-remote ()
  "`file-exists-p' is true for existing remote paths and false otherwise."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (should (file-exists-p (angelia-tests--remote "/etc/hostname")))
    (should-not (file-exists-p (angelia-tests--remote
                                "/non/existent/path/xyz-12345")))))

(ert-deftest test-dired-remote ()
  "`directory-files' on a remote directory returns the entries.
Full `dired-noselect' against a remote URL is not supported yet (would
need `unhandled-file-name-directory' + friends); this test covers the
file/list-dir RPC that backs any future dired support."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let ((files (directory-files (angelia-tests--remote "/tmp")
                                  nil nil 'nosort)))
      (should (listp files))
      (should (> (length files) 0))
      (should (member "." files))
      (should (member ".." files)))))

(ert-deftest test-dired-remote-count ()
  "`directory-files' honours its COUNT argument on remote paths."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let ((all (directory-files (angelia-tests--remote "/tmp")))
          (two (directory-files (angelia-tests--remote "/tmp") nil nil nil 2)))
      ;; "/tmp" always holds at least "." and ".." plus the setup files.
      (should (> (length all) 2))
      (should (= 2 (length two)))
      ;; Both calls sort, so COUNT must take the first names of the same order.
      (should (equal two (cl-subseq all 0 2))))))

(ert-deftest test-mkdir-remote ()
  "`make-directory' on a remote URL creates the directory on disk."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((local (expand-file-name
                   (format "angelia-test-mkdir-%d" (random 999999))
                   temporary-file-directory))
           (remote (angelia-tests--remote local)))
      (unwind-protect
          (progn
            (should-not (file-exists-p remote))
            (make-directory remote)
            (should (file-directory-p local))
            (should (file-directory-p remote)))
        (when (file-directory-p local) (delete-directory local t))))))

(ert-deftest test-delete-file-remote ()
  "`delete-file' on a remote URL removes the underlying file."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let ((local (make-temp-file "angelia-test-delete-")))
      (unwind-protect
          (let ((remote (angelia-tests--remote local)))
            (should (file-exists-p remote))
            (delete-file remote)
            (should-not (file-exists-p local)))
        (when (file-exists-p local) (delete-file local))))))

(ert-deftest test-large-file ()
  "Write a ~1 MB file via the handler, read it back, verify SHA256 match."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((local (make-temp-file "angelia-large-"))
           (size (* 1024 1024))
           (content
            (let ((s (make-string size ?x)))
              (aset s 0 ?A)
              (aset s (1- size) ?Z)
              s))
           (expected (secure-hash 'sha256 content)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert content)
              (write-region (point-min) (point-max)
                            (angelia-tests--remote local)
                            nil 'silent))
            (let ((got
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents (angelia-tests--remote local))
                     (secure-hash 'sha256 (current-buffer)))))
              (should (equal got expected))))
        (when (file-exists-p local) (delete-file local))))))

(ert-deftest test-very-large-file ()
  "Write a 50 MB file via chunked transfer and verify SHA256 round-trip.
With the default 64 KB chunk size this exercises ~800 chunks each way --
validates the chunk loop under volume and the absence of byte loss."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (let* ((local (make-temp-file "angelia-very-large-"))
           (size (* 50 1024 1024))                           ; 50 MB
           ;; Single ASCII byte so the buffer is unibyte-clean; sentinels at
           ;; the boundaries detect bytes silently swapped or dropped.
           (content (let ((s (make-string size ?x)))
                      (aset s 0 ?A)
                      (aset s (/ size 2) ?M)
                      (aset s (1- size) ?Z)
                      s))
           (expected (secure-hash 'sha256 content)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert content)
              (write-region (point-min) (point-max)
                            (angelia-tests--remote local)
                            nil 'silent))
            (let ((got
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents (angelia-tests--remote local))
                     (secure-hash 'sha256 (current-buffer)))))
              (should (equal got expected))))
        (when (file-exists-p local) (delete-file local))))))

;; ---------------------------------------------------------------------------
;; File-handler completeness + remote edit locking (Step 5).

(ert-deftest test-file-equal-p-remote ()
  "`file-equal-p' is true for the same remote path, false for different ones.
\(The default delegation wrongly returns t for any two angelia paths.)"
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (angelia-tests--with-remote-temp-file a "a\n"
      (angelia-tests--with-remote-temp-file b "b\n"
        (let ((ra (angelia-tests--remote a))
              (rb (angelia-tests--remote b)))
          (should (file-equal-p ra ra))
          (should-not (file-equal-p ra rb)))))))

(ert-deftest test-file-newer-than-remote ()
  "`file-newer-than-file-p' reflects the remote mtimes."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (angelia-tests--with-remote-temp-file old "old\n"
      (angelia-tests--with-remote-temp-file new "new\n"
        ;; Force a clear mtime ordering regardless of filesystem granularity.
        (set-file-times old (time-subtract (current-time) 100))
        (let ((rold (angelia-tests--remote old))
              (rnew (angelia-tests--remote new)))
          (should (file-newer-than-file-p rnew rold))
          (should-not (file-newer-than-file-p rold rnew))
          ;; Newer than an absent file is always true.
          (should (file-newer-than-file-p
                   rnew (angelia-tests--remote "/no/such/file/xyz"))))))))

(ert-deftest test-access-file-remote ()
  "`access-file' returns nil for a readable file and signals for a missing one."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (angelia-tests--with-remote-temp-file f "x\n"
      (should (null (access-file (angelia-tests--remote f) "open")))
      (should-error
       (access-file (angelia-tests--remote "/no/such/path/xyz-9") "open")))))

(ert-deftest test-file-locks-roundtrip ()
  "lock-file / file-locked-p / unlock-file round-trip, and a foreign owner is
reported as a string (not t)."
  (angelia-tests--file-ops-setup)
  (with-angelia-connection angelia-tests--target-host _conn
    (angelia-tests--with-remote-temp-file f "x\n"
      (let ((url (angelia-tests--remote f)))
        ;; Unlocked to start.
        (should (null (file-locked-p url)))
        ;; We take the lock -> reported as ours (t).
        (lock-file url)
        (should (eq t (file-locked-p url)))
        ;; A foreign owner is reported verbatim, not as t.
        (angelia-client-call angelia-tests--target-host 'file/lock
                             (angelia-client-files--params
                              "path" f "owner" "someone@elsewhere.999"))
        (should (equal (file-locked-p url) "someone@elsewhere.999"))
        ;; Re-take and release.
        (lock-file url)
        (unlock-file url)
        (should (null (file-locked-p url)))))))

;;; test-file-ops.el ends here
