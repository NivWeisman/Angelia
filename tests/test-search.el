;;; -*- lexical-binding: t; -*-
;;; test-search.el --- Layer 2 tests for remote search (file/search)

;; Exercises the streaming `file/search' RPC end-to-end: the server runs
;; rg/grep on the real tree and streams hits, which `angelia-client-files-search'
;; collects into (FILE LINE COL TEXT) tuples (FILE an Angelia URL).  Also smoke-
;; tests the interactive `angelia-grep' results buffer.
;;
;; Requires passwordless `ssh localhost' and either `rg' or `grep' on the host.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)

(defconst angelia-tests--search-host "localhost")

(defun angelia-tests--search-remote (path)
  (concat "/@angelia:" angelia-tests--search-host ":" path))

(defmacro angelia-tests--with-search-tree (dir-var &rest body)
  "Bind DIR-VAR to a fresh tree with a few seeded files, run BODY, clean up."
  (declare (indent 1))
  `(let ((,dir-var (file-name-as-directory (make-temp-file "angelia-search-" t))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,dir-var) (delete-directory ,dir-var t)))))

;; ---------------------------------------------------------------------------

(ert-deftest test-search-finds-matches ()
  "A unique token planted in two files is found with file/line/text."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--search-host _conn
    (angelia-tests--with-search-tree dir
      (angelia-tests-write-file (expand-file-name "a.txt" dir)
                                "nothing here\nZZTOKENZZ on line two\n")
      (make-directory (expand-file-name "sub" dir))
      (angelia-tests-write-file (expand-file-name "sub/b.txt" dir)
                                "ZZTOKENZZ at the top\nmore\n")
      (let* ((matches (angelia-client-files-search
                       angelia-tests--search-host dir "ZZTOKENZZ")))
        (should (= 2 (length matches)))
        ;; Every hit is an Angelia URL with a positive line and the token text.
        (dolist (m matches)
          (should (string-prefix-p "/@angelia:" (nth 0 m)))
          (should (integerp (nth 1 m)))
          (should (> (nth 1 m) 0))
          (should (string-match-p "ZZTOKENZZ" (nth 3 m))))
        ;; Both seeded files show up.
        (let ((files (mapcar (lambda (m) (file-name-nondirectory (nth 0 m)))
                             matches)))
          (should (member "a.txt" files))
          (should (member "b.txt" files)))))))

(ert-deftest test-search-no-match ()
  "Searching for an absent token yields an empty result, cleanly."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--search-host _conn
    (angelia-tests--with-search-tree dir
      (angelia-tests-write-file (expand-file-name "a.txt" dir) "just some text\n")
      (should (null (angelia-client-files-search
                     angelia-tests--search-host dir "NOPE_NOT_PRESENT_XYZ"))))))

(ert-deftest test-search-respects-cap ()
  "A small MAX caps the number of streamed matches."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--search-host _conn
    (angelia-tests--with-search-tree dir
      (angelia-tests-write-file
       (expand-file-name "many.txt" dir)
       (mapconcat (lambda (i) (format "CAPTOKEN line %d" i))
                  (number-sequence 1 60) "\n"))
      (let ((matches (angelia-client-files-search
                      angelia-tests--search-host dir "CAPTOKEN" 10)))
        (should (> (length matches) 0))
        (should (<= (length matches) 10))))))

(ert-deftest test-search-angelia-grep-buffer ()
  "`angelia-grep' streams parseable hit lines into its results buffer."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--search-host _conn
    (angelia-tests--with-search-tree dir
      (angelia-tests-write-file (expand-file-name "c.txt" dir)
                                "first\nGREPTOKEN here\n")
      (let ((buf (angelia-grep (angelia-tests--search-remote dir) "GREPTOKEN")))
        (unwind-protect
            (progn
              ;; Pump until the streamed match line lands.
              (with-timeout (15 nil)
                (while (not (with-current-buffer buf
                              (save-excursion
                                (goto-char (point-min))
                                (re-search-forward "c\\.txt:[0-9]+:" nil t))))
                  (accept-process-output nil 0.1)))
              (with-current-buffer buf
                (should (string-match-p "GREPTOKEN" (buffer-string)))
                (should (string-match-p "c\\.txt:[0-9]+:" (buffer-string)))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; test-search.el ends here
