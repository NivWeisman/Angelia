;;; -*- lexical-binding: t; -*-
;;; test-dired.el --- Layer 2 tests for dired primitives on Angelia paths

;; Covers the four file ops dired's editing commands (delete / rename / copy)
;; actually exercise, plus the `dired-noselect' end-to-end smoke that
;; `insert-directory' + `directory-files-and-attributes' produce a usable
;; listing buffer.  Doesn't try to drive the dired keymap; the underlying
;; primitives are what we care about and what magit / others will reuse.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)

(defconst angelia-tests--dired-host "localhost")

(defun angelia-tests--dired-remote (path)
  (concat "/@angelia:" angelia-tests--dired-host ":" path))

(defmacro angelia-tests--with-dired-tmpdir (var &rest body)
  (declare (indent 1))
  `(let ((,var (file-name-as-directory
                (make-temp-file "angelia-dired-" t))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,var) (delete-directory ,var t)))))

;; ---------------------------------------------------------------------------

(ert-deftest test-dired-copy-file ()
  "`copy-file' between two angelia paths replicates content."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (let* ((src (expand-file-name "src.txt" dir))
             (dst (expand-file-name "dst.txt" dir))
             (src-r (angelia-tests--dired-remote src))
             (dst-r (angelia-tests--dired-remote dst)))
        (angelia-tests-write-file src "copy me\n")
        (copy-file src-r dst-r)
        (should (file-exists-p dst))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally dst)
          (should (equal (buffer-string) "copy me\n")))))))

(ert-deftest test-dired-rename-file ()
  "`rename-file' between two angelia paths moves the underlying file."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (let* ((src (expand-file-name "src.txt" dir))
             (dst (expand-file-name "renamed.txt" dir))
             (src-r (angelia-tests--dired-remote src))
             (dst-r (angelia-tests--dired-remote dst)))
        (angelia-tests-write-file src "rename me\n")
        (rename-file src-r dst-r)
        (should-not (file-exists-p src))
        (should (file-exists-p dst))))))

(ert-deftest test-dired-delete-directory-recursive ()
  "`delete-directory' RECURSIVE drops a populated directory subtree."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (let* ((root (file-name-as-directory
                  (make-temp-file "angelia-dired-rm-" t)))
           (root-r (angelia-tests--dired-remote root)))
      (angelia-tests-write-file (expand-file-name "a" root) "x")
      (make-directory (expand-file-name "sub" root))
      (angelia-tests-write-file (expand-file-name "sub/b" root) "y")
      (delete-directory root-r t)
      (should-not (file-directory-p root)))))

(ert-deftest test-dired-directory-files-and-attributes ()
  "`directory-files-and-attributes' returns per-entry attr tuples."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (angelia-tests-write-file (expand-file-name "alpha" dir) "1")
      (make-directory (expand-file-name "subdir" dir))
      (let* ((entries (directory-files-and-attributes
                       (angelia-tests--dired-remote dir)
                       nil nil 'nosort)))
        (should (assoc "alpha" entries))
        (should (assoc "subdir" entries))
        (let ((alpha-attrs (cdr (assoc "alpha" entries)))
              (sub-attrs (cdr (assoc "subdir" entries))))
          (should (null (car alpha-attrs)))    ; regular file -> nil
          (should (eq (car sub-attrs) t)))))))   ; directory -> t

(ert-deftest test-dired-insert-directory ()
  "`insert-directory' inserts a recognizable ls listing."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (angelia-tests-write-file (expand-file-name "marker.txt" dir) "x")
      (with-temp-buffer
        (insert-directory (angelia-tests--dired-remote dir)
                          "-la" nil t)
        (let ((text (buffer-string)))
          (should (string-match-p "marker\\.txt" text))
          (should (string-match-p "^total " text)))))))

(ert-deftest test-dired-noselect-on-remote ()
  "`dired-noselect' against a remote dir produces a buffer naming entries."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (angelia-tests-write-file (expand-file-name "dired-test.txt" dir) "x")
      (let ((buf (dired-noselect (angelia-tests--dired-remote dir))))
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "dired-test\\.txt" (buffer-string))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest test-file-name-all-completions ()
  "`file-name-all-completions' on a remote dir filters by prefix."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (angelia-tests--with-dired-tmpdir dir
      (angelia-tests-write-file (expand-file-name "apple"  dir) "")
      (angelia-tests-write-file (expand-file-name "apricot" dir) "")
      (angelia-tests-write-file (expand-file-name "banana" dir) "")
      (let ((matches (file-name-all-completions
                      "ap" (angelia-tests--dired-remote dir))))
        (should (member "apple" matches))
        (should (member "apricot" matches))
        (should-not (member "banana" matches))))))

(ert-deftest test-file-writable-p-rpc ()
  "`file-writable-p' is true for a writable file and false under a read-only dir."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--dired-host _conn
    (let* ((dir (file-name-as-directory
                 (make-temp-file "angelia-fwp-" t)))
           (writable (expand-file-name "w" dir)))
      (unwind-protect
          (progn
            (angelia-tests-write-file writable "x")
            (should (file-writable-p
                     (angelia-tests--dired-remote writable)))
            ;; Drop the parent dir's write bit so a new file there
            ;; would be denied.  The file itself remains writable.
            (set-file-modes dir #o555)
            (should-not
             (file-writable-p
              (angelia-tests--dired-remote
               (expand-file-name "fresh" dir)))))
        (set-file-modes dir #o755)
        (when (file-directory-p dir) (delete-directory dir t))))))

;;; test-dired.el ends here
