;;; -*- lexical-binding: t; -*-
;;; test-lsp.el --- Tests for angelia-client-lsp

;; Layer 0: URI mapping round-trips (no SSH).
;; Layer 2: process spawning and cleanup lifecycle (SSH localhost).

(require 'ert)
(require 'cl-lib)
(require 'test-helpers)

(let ((lsp-path (expand-file-name "angelia-client-lsp.el" angelia-tests--lisp-dir)))
  (when (file-exists-p lsp-path) (load lsp-path nil t)))

;; ---------------------------------------------------------------------------
;; Layer 0 — URI mapping (pure, no SSH).

(ert-deftest test-lsp-path-to-uri ()
  "/@angelia:HOST:/path converts to file:///path."
  (should (equal (angelia-client-lsp--path-to-uri "/@angelia:user@host:/foo/bar.py")
                 "file:///foo/bar.py"))
  (should (equal (angelia-client-lsp--path-to-uri "/@angelia:localhost:/src/main.rs")
                 "file:///src/main.rs"))
  ;; Nested directories.
  (should (equal (angelia-client-lsp--path-to-uri "/@angelia:host:/a/b/c/d.go")
                 "file:///a/b/c/d.go")))

(ert-deftest test-lsp-uri-to-path ()
  "file:///path converts back to /@angelia:HOST:/path with supplied host."
  (should (equal (angelia-client-lsp--uri-to-path "user@host" "file:///foo/bar.py")
                 "/@angelia:user@host:/foo/bar.py"))
  (should (equal (angelia-client-lsp--uri-to-path "localhost" "file:///src/main.rs")
                 "/@angelia:localhost:/src/main.rs")))

(ert-deftest test-lsp-uri-roundtrip ()
  "path-to-uri and uri-to-path are inverse for Angelia paths."
  (let* ((host "user@remote.example.com")
         (orig "/@angelia:user@remote.example.com:/project/src/lib.py")
         (uri  (angelia-client-lsp--path-to-uri orig)))
    (should (equal (angelia-client-lsp--uri-to-path host uri) orig))))

(ert-deftest test-lsp-non-angelia-path-returns-nil ()
  "`angelia-client-lsp--path-to-uri' returns nil for non-Angelia paths."
  (should (null (angelia-client-lsp--path-to-uri "/local/path/file.py")))
  (should (null (angelia-client-lsp--path-to-uri "C:/Windows/file.el")))
  (should (null (angelia-client-lsp--path-to-uri ""))))

(ert-deftest test-lsp-non-file-uri-returns-nil ()
  "`angelia-client-lsp--uri-to-path' returns nil for non-file:// URIs."
  (should (null (angelia-client-lsp--uri-to-path "host" "untitled:///unsaved")))
  (should (null (angelia-client-lsp--uri-to-path "host" "http://example.com/foo"))))

;; ---------------------------------------------------------------------------
;; Layer 0 — launch command routes through login-wrap (no SSH; make-process and
;; the shell-family probe are both mocked).

(ert-deftest test-lsp-make-process-uses-login-wrap ()
  "The LSP subprocess command is built via `angelia-client--login-wrap', not a
hardcoded `bash --login -c' (CLAUDE.md rule 5).  On a zsh host that distinction
is load-bearing: `bash --login' never sources ~/.zprofile, so the language
server would not be on PATH."
  (let (captured)
    (cl-letf (((symbol-function 'angelia-client--detect-shell-family)
               (lambda (_host) 'zsh))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured (plist-get args :command))
                 'dummy-proc)))
      (let* ((host "someuser@zsh-host")
             (cmd "pylsp")
             (proc (angelia-client-lsp--make-process host cmd))
             (remote-arg (nth 2 captured)))
        (should (eq proc 'dummy-proc))
        (should (equal (nth 0 captured) angelia-client-ssh-program))
        (should (equal (nth 1 captured) host))
        ;; The remote command is exactly login-wrap's output...
        (should (equal remote-arg (angelia-client--login-wrap host cmd)))
        ;; ...which on a zsh host sources ~/.zprofile and execs bash, and never
        ;; uses the broken hardcoded `bash --login'.
        (should (string-match-p "zprofile" remote-arg))
        (should (string-match-p "exec bash -c" remote-arg))
        (should-not (string-match-p "bash --login" remote-arg))))))

;; ---------------------------------------------------------------------------
;; Layer 2 — process lifecycle (SSH localhost).

(defconst angelia-tests--lsp-host "localhost"
  "SSH target for LSP process lifecycle tests.")

(ert-deftest test-lsp-process-spawns ()
  "`angelia-client-lsp--make-process' returns a live pipe process."
  (let ((proc (angelia-client-lsp--make-process angelia-tests--lsp-host "cat")))
    (unwind-protect
        (progn
          (should (processp proc))
          (should (process-live-p proc))
          (should (equal (process-type proc) 'real)))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest test-lsp-process-cleanup ()
  "`angelia-client-lsp--cleanup-host' kills all registered processes for the host."
  (let ((proc (angelia-client-lsp--make-process angelia-tests--lsp-host "cat")))
    (unwind-protect
        (progn
          (angelia-client-lsp--register-process angelia-tests--lsp-host proc)
          (should (member proc
                          (gethash angelia-tests--lsp-host
                                   angelia-client-lsp--processes)))
          (angelia-client-lsp--cleanup-host angelia-tests--lsp-host)
          ;; Process should be dead and removed from the registry.
          (should (not (process-live-p proc)))
          (should (null (gethash angelia-tests--lsp-host
                                 angelia-client-lsp--processes))))
      ;; Belt-and-suspenders cleanup in case the test fails mid-way.
      (when (process-live-p proc) (delete-process proc))
      (remhash angelia-tests--lsp-host angelia-client-lsp--processes))))

(ert-deftest test-lsp-sentinel-deregisters ()
  "Process sentinel removes the process from the registry when it exits."
  (let ((proc (angelia-client-lsp--make-process angelia-tests--lsp-host "true")))
    (angelia-client-lsp--register-process angelia-tests--lsp-host proc)
    ;; Wait for `true' to exit and the sentinel to fire.
    (with-timeout (5 (error "timed out waiting for process exit"))
      (while (process-live-p proc)
        (accept-process-output nil 0.05)))
    ;; The sentinel should have removed it.
    (should (not (member proc
                         (gethash angelia-tests--lsp-host
                                  angelia-client-lsp--processes))))
    (remhash angelia-tests--lsp-host angelia-client-lsp--processes)))

;;; test-lsp.el ends here
