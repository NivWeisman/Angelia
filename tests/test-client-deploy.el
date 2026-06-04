;;; -*- lexical-binding: t; -*-
;;; test-client-deploy.el --- Layer 0 unit tests for angelia-client-deploy.el

;; Pure, SSH-free tests for the client deploy helpers.  We seed the per-host
;; shell-family cache directly so `angelia-client--login-wrap' never probes a
;; real host, which keeps these at Layer 0.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client-deploy)

;; ---------------------------------------------------------------------------
;; angelia-client--detect-shell-family classification.
;;
;; We can't run the real SSH probe at Layer 0, but the classification logic
;; lives in a single `cond' on the trimmed $SHELL string; exercise it by
;; faking the probe via `cl-letf' on `angelia-client--ssh-run-raw'.

(defmacro angelia-client-tests--with-shell ($shell &rest body)
  "Run BODY with the remote $SHELL probe stubbed to return $SHELL, cache cleared."
  (declare (indent 1))
  `(let ((angelia-client--remote-shell-family (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'angelia-client--ssh-run-raw)
                (lambda (&rest _) (list :exit 0 :stdout ,$shell :stderr ""))))
       ,@body)))

(ert-deftest test-client-detect-shell-family ()
  "$SHELL basenames classify into csh / zsh / sh families."
  (angelia-client-tests--with-shell "/bin/tcsh"
    (should (eq 'csh (angelia-client--detect-shell-family "h"))))
  (angelia-client-tests--with-shell "/bin/csh"
    (should (eq 'csh (angelia-client--detect-shell-family "h"))))
  (angelia-client-tests--with-shell "/bin/zsh"
    (should (eq 'zsh (angelia-client--detect-shell-family "h"))))
  (angelia-client-tests--with-shell "/usr/local/bin/zsh\n"
    (should (eq 'zsh (angelia-client--detect-shell-family "h"))))
  (angelia-client-tests--with-shell "/bin/bash"
    (should (eq 'sh (angelia-client--detect-shell-family "h"))))
  (angelia-client-tests--with-shell "/bin/ksh"
    (should (eq 'sh (angelia-client--detect-shell-family "h"))))
  ;; zsh must win over csh's suffix match (neither "zsh" ends in "csh").
  (angelia-client-tests--with-shell "/opt/homebrew/bin/zsh"
    (should (eq 'zsh (angelia-client--detect-shell-family "h")))))

(ert-deftest test-client-detect-shell-family-cached ()
  "The family is cached per host: a second call does not re-probe."
  (let ((angelia-client--remote-shell-family (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'angelia-client--ssh-run-raw)
               (lambda (&rest _) (cl-incf calls) (list :exit 0 :stdout "/bin/zsh" :stderr ""))))
      (should (eq 'zsh (angelia-client--detect-shell-family "h")))
      (should (eq 'zsh (angelia-client--detect-shell-family "h")))
      (should (= 1 calls)))))

;; ---------------------------------------------------------------------------
;; angelia-client--login-wrap per family.

(defun angelia-client-tests--wrap-for (family command)
  "Return `angelia-client--login-wrap' output for COMMAND with FAMILY seeded."
  (let ((angelia-client--remote-shell-family (make-hash-table :test #'equal)))
    (puthash "h" family angelia-client--remote-shell-family)
    (angelia-client--login-wrap "h" command)))

(ert-deftest test-client-login-wrap-sh ()
  "sh-family hosts use bash's own --login."
  (let ((w (angelia-client-tests--wrap-for 'sh "emacs --batch")))
    (should (string-prefix-p "bash --login -c " w))
    (should (string-match-p "emacs --batch" w))))

(ert-deftest test-client-login-wrap-csh ()
  "csh-family hosts source the csh login files, then exec bash."
  (let ((w (angelia-client-tests--wrap-for 'csh "emacs --batch")))
    (should (string-match-p "source /etc/csh.login" w))
    (should (string-match-p "source ~/.login" w))
    (should (string-match-p "exec bash -c " w))
    ;; Login-file noise is redirected so it can't reach the JSON-RPC stdout.
    (should (string-match-p "/etc/csh.login >& /dev/null" w))))

(ert-deftest test-client-login-wrap-zsh ()
  "zsh-family hosts source the zsh login profiles, then exec bash.
This is the macOS-default-shell path: /opt/homebrew/bin reaches PATH only via
~/.zprofile, which `bash --login' never sources."
  (let ((w (angelia-client-tests--wrap-for 'zsh "emacs --batch")))
    (should (string-match-p "source /etc/zprofile" w))
    (should (string-match-p "source ~/.zprofile" w))
    (should (string-match-p "exec bash -c " w))
    ;; Each source silences its own output to keep stdout sacred.
    (should (string-match-p "/etc/zprofile >/dev/null 2>&1" w))
    (should (string-match-p "~/.zprofile >/dev/null 2>&1" w))
    ;; It must NOT fall back to bash --login (the bug we're fixing).
    (should-not (string-match-p "bash --login" w))))

(ert-deftest test-client-login-wrap-quotes-command ()
  "The bash command is single-quoted so its $vars survive the outer shell."
  (let ((w (angelia-client-tests--wrap-for 'zsh "X=\"$f\" emacs")))
    ;; The literal $f must be inside single quotes (not expanded by zsh).
    (should (string-match-p "exec bash -c 'X=\"\\$f\" emacs'" w))))

;;; test-client-deploy.el ends here
