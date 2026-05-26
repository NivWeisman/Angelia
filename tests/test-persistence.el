;;; -*- lexical-binding: t; -*-
;;; test-persistence.el --- Layer 2 persistence tests (dtach / tmux / screen)

;; Each per-backend test starts with `(skip-unless (executable-find ...))'
;; so the suite cleanly skips on machines where the backend isn't
;; installed.  Names use a random suffix so concurrent test runs (or
;; leftover state from a previous failed run) don't collide.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-proc)

(defconst angelia-tests--persist-host "localhost"
  "Single-VM test target for persistence tests.")

(defconst angelia-tests--persist-backends
  '((dtach  . "dtach")
    (tmux   . "tmux")
    (screen . "screen"))
  "Map backend symbol -> binary name probed via `executable-find'.")

(defun angelia-tests--unique-persist-name (suffix)
  "Return a short, unique persistence name suitable for one test."
  (format "tp-%s-%06x" suffix (random (* 16 1024 1024))))

(defmacro angelia-tests--with-persist (handle-var binding &rest body)
  "Spawn a persisted bash on `angelia-tests--persist-host'.
BINDING is `(BACKEND PERSIST-NAME)'; HANDLE-VAR is bound to the resulting
proc handle, and an unwind-protect tears the persistence down via
`angelia-client-proc-kill-persisted' at the end."
  (declare (indent 2))
  (let ((backend-sym (car binding))
        (name-sym (cadr binding)))
    `(let ((,handle-var nil))
       (unwind-protect
           (progn
             (setq ,handle-var
                   (angelia-client-proc-start
                    angelia-tests--persist-host
                    (list "bash" "--norc" "--noprofile" "-i")
                    :persist ,name-sym
                    :backend ,backend-sym))
             ,@body)
         (when ,handle-var
           (ignore-errors (angelia-client-proc-close ,handle-var)))
         (ignore-errors
           (angelia-client-proc-kill-persisted
            angelia-tests--persist-host ,name-sym ,backend-sym))))))

(defun angelia-tests--wait-for-match (getter pattern &optional timeout)
  "Pump events until (funcall GETTER) matches PATTERN, or signal on timeout.
GETTER is a closure that returns the current accumulated string; passing a
closure (instead of a symbol-name) keeps this working under
lexical-binding, where `symbol-value' cannot reach a let-bound variable."
  (with-timeout ((or timeout 5)
                 (error "timeout waiting for %S; last value: %S"
                        pattern (funcall getter)))
    (while (not (string-match-p pattern (funcall getter)))
      (accept-process-output nil 0.05))))

;; ---------------------------------------------------------------------------
;; Per-backend lifecycle: spawn -> echo -> detach -> reattach -> echo again.

(defmacro angelia-tests--define-per-backend (suffix docstring &rest body)
  "Generate one `ert-deftest' per known persistence backend.
Each generated body runs with `backend' bound to the backend symbol and
`backend-binary' bound to its executable name.  Skipped when the binary
isn't installed."
  (declare (indent 2))
  `(progn
     ,@(mapcar
        (lambda (entry)
          (let* ((sym (car entry))
                 (bin (cdr entry))
                 (tname (intern (format "test-persist-%s-%s" suffix sym))))
            `(ert-deftest ,tname ()
               ,docstring
               (skip-unless (executable-find ,bin))
               (angelia-tests-ensure-no-connections)
               (with-angelia-connection angelia-tests--persist-host _conn
                 (let ((backend ',sym)
                       (backend-binary ,bin))
                   ,@body)))))
        angelia-tests--persist-backends)))

(angelia-tests--define-per-backend "spawn-detach-reattach"
    "Persisted spawn + send echo + detach + reattach + send + verify."
  (let* ((pname (angelia-tests--unique-persist-name "lifecycle"))
         (out-a "")
         (out-b ""))
    (unwind-protect
        (progn
          (let ((handle (angelia-client-proc-start
                         angelia-tests--persist-host
                         (list "bash" "--norc" "--noprofile" "-i")
                         :persist pname
                         :backend backend
                         :on-output (lambda (b) (setq out-a (concat out-a b))))))
            ;; Tiny pause for bash to print its prompt.
            (sleep-for 0.3)
            (angelia-client-proc-send handle "echo MARK_ALPHA\n")
            (angelia-tests--wait-for-match (lambda () out-a) "MARK_ALPHA" 5)
            (angelia-client-proc-detach handle))
          ;; The persisted name should still be listed.
          (let ((listing (angelia-client-proc-list-persisted
                          angelia-tests--persist-host backend)))
            (should (cl-some (lambda (s) (equal (plist-get s :name) pname))
                             listing)))
          (let ((re-handle
                 (angelia-client-proc-reattach
                  angelia-tests--persist-host pname backend
                  :on-output (lambda (b) (setq out-b (concat out-b b))))))
            (sleep-for 0.3)
            (angelia-client-proc-send re-handle "echo MARK_BETA\n")
            (angelia-tests--wait-for-match (lambda () out-b) "MARK_BETA" 5)
            (angelia-client-proc-detach re-handle)))
      (ignore-errors
        (angelia-client-proc-kill-persisted
         angelia-tests--persist-host pname backend)))))

(angelia-tests--define-per-backend "survives-disconnect"
    "Persisted process survives an SSH disconnect + fresh reconnect."
  (let* ((pname (angelia-tests--unique-persist-name "disco")))
    (unwind-protect
        (progn
          (let ((handle (angelia-client-proc-start
                         angelia-tests--persist-host
                         (list "bash" "--norc" "--noprofile" "-i")
                         :persist pname
                         :backend backend)))
            (sleep-for 0.3)
            (angelia-client-proc-detach handle))
          ;; Tear the connection down completely; reconnect; the persisted
          ;; session must still be visible.
          (angelia-client-disconnect angelia-tests--persist-host)
          (angelia-client-connect angelia-tests--persist-host)
          (let ((listing (angelia-client-proc-list-persisted
                          angelia-tests--persist-host backend)))
            (should (cl-some (lambda (s) (equal (plist-get s :name) pname))
                             listing))))
      (ignore-errors
        (angelia-client-proc-kill-persisted
         angelia-tests--persist-host pname backend)))))

;; ---------------------------------------------------------------------------
;; Cross-backend list / kill / detach-without-kill / default.

(ert-deftest test-persist-list-and-kill-mixed ()
  "With >= 2 backends installed, list across all then kill one and re-list."
  (let* ((available
          (cl-remove-if-not
           (lambda (e) (executable-find (cdr e)))
           angelia-tests--persist-backends)))
    (skip-unless (>= (length available) 2))
    (angelia-tests-ensure-no-connections)
    (with-angelia-connection angelia-tests--persist-host _conn
      (let* ((entry-a (nth 0 available))
             (entry-b (nth 1 available))
             (name-a (angelia-tests--unique-persist-name "mix-a"))
             (name-b (angelia-tests--unique-persist-name "mix-b"))
             (handle-a nil) (handle-b nil))
        (unwind-protect
            (progn
              (setq handle-a
                    (angelia-client-proc-start
                     angelia-tests--persist-host
                     (list "bash" "--norc" "--noprofile" "-i")
                     :persist name-a :backend (car entry-a)))
              (setq handle-b
                    (angelia-client-proc-start
                     angelia-tests--persist-host
                     (list "bash" "--norc" "--noprofile" "-i")
                     :persist name-b :backend (car entry-b)))
              (sleep-for 0.3)
              (angelia-client-proc-detach handle-a)
              (angelia-client-proc-detach handle-b)
              (let ((all (angelia-client-proc-list-persisted
                          angelia-tests--persist-host)))
                (should (cl-some (lambda (s)
                                   (and (equal (plist-get s :name) name-a)
                                        (equal (plist-get s :backend)
                                               (symbol-name (car entry-a)))))
                                 all))
                (should (cl-some (lambda (s)
                                   (and (equal (plist-get s :name) name-b)
                                        (equal (plist-get s :backend)
                                               (symbol-name (car entry-b)))))
                                 all)))
              ;; Kill A, verify B survives the listing.
              (angelia-client-proc-kill-persisted
               angelia-tests--persist-host name-a (car entry-a))
              ;; Give the backend a moment to drop the entry.
              (sleep-for 0.3)
              (let ((after (angelia-client-proc-list-persisted
                            angelia-tests--persist-host)))
                (should-not (cl-some (lambda (s)
                                       (equal (plist-get s :name) name-a))
                                     after))
                (should (cl-some (lambda (s) (equal (plist-get s :name) name-b))
                                 after))))
          (ignore-errors
            (angelia-client-proc-kill-persisted
             angelia-tests--persist-host name-a (car entry-a)))
          (ignore-errors
            (angelia-client-proc-kill-persisted
             angelia-tests--persist-host name-b (car entry-b))))))))

(ert-deftest test-persist-detach-without-kill ()
  "Detaching a persisted session leaves it alive in the listing."
  (skip-unless (executable-find "dtach"))
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--persist-host _conn
    (let* ((pname (angelia-tests--unique-persist-name "noKill"))
           (handle nil))
      (unwind-protect
          (progn
            (setq handle
                  (angelia-client-proc-start
                   angelia-tests--persist-host
                   (list "bash" "--norc" "--noprofile" "-i")
                   :persist pname :backend 'dtach))
            (sleep-for 0.3)
            (angelia-client-proc-detach handle)
            (let ((listing (angelia-client-proc-list-persisted
                            angelia-tests--persist-host 'dtach)))
              (should (cl-some (lambda (s)
                                 (equal (plist-get s :name) pname))
                               listing))))
        (ignore-errors
          (angelia-client-proc-kill-persisted
           angelia-tests--persist-host pname 'dtach))))))

(ert-deftest test-persist-default-backend ()
  "Passing `:persist' without `:backend' picks dtach (the documented default)."
  (skip-unless (executable-find "dtach"))
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--persist-host _conn
    (let* ((pname (angelia-tests--unique-persist-name "default"))
           (handle nil))
      (unwind-protect
          (progn
            (setq handle (angelia-client-proc-start
                          angelia-tests--persist-host
                          (list "bash" "--norc" "--noprofile" "-i")
                          :persist pname))    ; no :backend
            (sleep-for 0.3)
            (angelia-client-proc-detach handle)
            (let ((all (angelia-client-proc-list-persisted
                        angelia-tests--persist-host)))
              (should
               (cl-some (lambda (s)
                          (and (equal (plist-get s :name) pname)
                               (equal (plist-get s :backend) "dtach")))
                        all))))
        (ignore-errors
          (angelia-client-proc-kill-persisted
           angelia-tests--persist-host pname 'dtach))))))

;;; test-persistence.el ends here
