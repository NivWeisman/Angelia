;;; -*- lexical-binding: t; -*-
;;; run-all.el --- ERT entry points for Angelia tests

;; This file ONLY defines helpers; running tests is the caller's job, via
;; `--eval' on the emacs command line.  Loading this file does not, by itself,
;; run any tests.  That separation lets the Makefile's per-layer targets work
;; without a wider default also firing.
;;
;;   make test            -> emacs ... -l run-all.el --eval '(angelia-tests-run-all)'
;;   make test-unit       -> emacs ... -l run-all.el --eval '(angelia-tests-run-layer 0)'
;;   make test-transport  -> emacs ... -l run-all.el --eval '(angelia-tests-run-layer 1)'
;;   make test-files      -> emacs ... -l run-all.el --eval '(angelia-tests-run-layer 2)'

(require 'ert)

(defvar angelia-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defun angelia-tests--load-file (name)
  "Load NAME from `angelia-tests--dir' if it exists."
  (let ((path (expand-file-name name angelia-tests--dir)))
    (when (file-exists-p path)
      (load path nil t))))

(defun angelia-tests-load-all ()
  "Load every test-*.el file under tests/, plus test-helpers.el first."
  (angelia-tests--load-file "test-helpers.el")
  (dolist (file (directory-files angelia-tests--dir t "\\`test-.*\\.el\\'"))
    (unless (string-suffix-p "test-helpers.el" file)
      (load file nil t))))

(defun angelia-tests-run-all ()
  "Load and run every test file; exit when done."
  (angelia-tests-load-all)
  (ert-run-tests-batch-and-exit))

(defun angelia-tests-run-layer (layer)
  "Run only the tests for LAYER (0, 1, or 2) and exit.
Layer 2 covers all SSH-localhost integration tests: file ops + PTY procs
+ (eventually) persistence."
  (angelia-tests--load-file "test-helpers.el")
  (pcase layer
    (0 (angelia-tests--load-file "test-server-unit.el"))
    (1 (angelia-tests--load-file "test-transport.el"))
    (2 (angelia-tests--load-file "test-file-ops.el")
       (angelia-tests--load-file "test-proc.el")
       (angelia-tests--load-file "test-persistence.el"))
    (_ (error "Unknown test layer: %S" layer)))
  (ert-run-tests-batch-and-exit))

;;; run-all.el ends here
