;;; -*- lexical-binding: t; -*-
;;; test-client-files-unit.el --- Layer 0 unit tests for angelia-client-files.el

;; Pure, SSH-free tests for URL parsing and remote-path normalization.  These
;; cover the tilde-path regression: a `~'-relative remote path must parse and
;; round-trip with the tilde *preserved* (the remote host resolves it, not us).

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client-files)

;; ---------------------------------------------------------------------------
;; angelia-client-files--parse: absolute AND home-relative remote paths.

(ert-deftest test-client-files-parse-absolute ()
  "An absolute remote path parses into (HOST . /path)."
  (should (equal '("h" . "/etc/hosts")
                 (angelia-client-files--parse "/@angelia:h:/etc/hosts")))
  ;; Hosts may contain colons (user@host:port); the non-greedy match plus the
  ;; leading-/ requirement on the remote keeps them whole.
  (should (equal '("u@host:2222" . "/srv/x")
                 (angelia-client-files--parse "/@angelia:u@host:2222:/srv/x"))))

(ert-deftest test-client-files-parse-tilde ()
  "A home-relative remote path parses with the tilde captured verbatim."
  (should (equal '("h" . "~/.cshrc")
                 (angelia-client-files--parse "/@angelia:h:~/.cshrc")))
  (should (equal '("h" . "~bob/notes")
                 (angelia-client-files--parse "/@angelia:h:~bob/notes")))
  (should (equal '("u@host:22" . "~/.cshrc")
                 (angelia-client-files--parse "/@angelia:u@host:22:~/.cshrc"))))

(ert-deftest test-client-files-parse-no-path ()
  "A bare `/@angelia:HOST:' (no path component) does not parse."
  (should (null (angelia-client-files--parse "/@angelia:h:")))
  (should (null (angelia-client-files--parse "/plain/local/path"))))

;; ---------------------------------------------------------------------------
;; angelia-client-files--normalize-remote (case a: re-wrapping a URL).

(ert-deftest test-client-files-normalize-remote ()
  "Absolute paths collapse `..'; tilde paths are left for the remote to expand."
  (should (equal "/a/c"      (angelia-client-files--normalize-remote "/a/b/../c")))
  ;; Trailing slash on a directory path is preserved.
  (should (equal "/etc/"     (angelia-client-files--normalize-remote "/etc/")))
  ;; Tildes must NOT be expanded locally -- the remote $HOME may differ.
  (should (equal "~/.cshrc"  (angelia-client-files--normalize-remote "~/.cshrc")))
  (should (equal "~bob/f"    (angelia-client-files--normalize-remote "~bob/f")))
  (should (equal "~/a/../b"  (angelia-client-files--normalize-remote "~/a/../b"))))

;; ---------------------------------------------------------------------------
;; angelia-client-files--join-remote (case b: relative NAME onto a remote DIR).

(ert-deftest test-client-files-join-remote ()
  "Relative names join onto remote dirs, preserving a leading tilde."
  (should (equal "/etc/x"        (angelia-client-files--join-remote "x" "/etc/")))
  (should (equal "~/sub/.cshrc"  (angelia-client-files--join-remote ".cshrc" "~/sub/")))
  (should (equal "~/x"           (angelia-client-files--join-remote "x" "~")))
  (should (equal "~bob/sub/x"    (angelia-client-files--join-remote "x" "~bob/sub"))))

;;; test-client-files-unit.el ends here
