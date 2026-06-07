;;; -*- lexical-binding: t; -*-
;;; test-config.el --- Layer 2 tests for server-side config injection

;; Pushes a dedicated config file into the remote server and checks it took
;; effect: a custom RPC method it registered answers, an LSP declaration it made
;; is readable, and -- crucially -- a `princ' in the config did NOT corrupt the
;; JSON-RPC stream (hard rule 1: stdout is sacred).  Also checks the config is
;; re-applied automatically across a reconnect.
;;
;; Requires passwordless `ssh localhost'.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-config)

(defconst angelia-tests--config-host "localhost")

;; A config that deliberately writes to stdout (to prove the guard), registers a
;; custom method, declares an LSP, and sets a server-side variable.
(defconst angelia-tests--config-body "\
;;; -*- lexical-binding: t; -*-
(princ \"THIS-MUST-NOT-REACH-STDOUT\\n\")
(print '(also not this))
(angelia-server-register-method \"custom/double\"
  (lambda (_conn params)
    (let ((n (gethash \"n\" params))
          (h (make-hash-table :test #'equal)))
      (puthash \"value\" (* 2 n) h)
      h)))
(angelia-server-register-lsp 'python-mode \"pylsp\")
(defvar angelia-test-project-root \"/srv/project\")
")

(defmacro angelia-tests--with-config-file (var &rest body)
  "Bind VAR to a temp file holding the test config, run BODY, delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "angelia-cfg-" nil ".el"
                               angelia-tests--config-body)))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun angelia-tests--call (host method &rest kv)
  "Call METHOD on HOST with a hash built from the KV plist."
  (angelia-client-call host method
                       (let ((h (make-hash-table :test #'equal)))
                         (while kv (puthash (pop kv) (pop kv) h))
                         h)))

;; ---------------------------------------------------------------------------

(ert-deftest test-config-load-registers-method ()
  "Loading the config registers a custom method that then answers correctly,
and a `princ' in the config does not corrupt the protocol."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--config-host _conn
    (angelia-tests--with-config-file cfg
      (let ((resp (angelia-client-load-server-config angelia-tests--config-host cfg)))
        (should (eq (plist-get resp :ok) t))
        (should (member "custom/double" (append (plist-get resp :methods) nil))))
      ;; The custom method works.
      (should (= 42 (plist-get (angelia-tests--call
                                angelia-tests--config-host 'custom/double "n" 21)
                               :value)))
      ;; stdout was NOT corrupted: a normal request still round-trips.
      (should (eq (plist-get (angelia-client-call
                              angelia-tests--config-host 'server/ping nil)
                             :pong)
                  t)))))

(ert-deftest test-config-declares-lsp ()
  "An `angelia-server-register-lsp' call in the config is readable via
server/lsp-programs."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--config-host _conn
    (angelia-tests--with-config-file cfg
      (angelia-client-load-server-config angelia-tests--config-host cfg)
      (let ((programs (plist-get (angelia-client-call
                                  angelia-tests--config-host 'server/lsp-programs nil)
                                 :programs)))
        (should (equal (plist-get programs :python-mode) "pylsp"))))))

(ert-deftest test-config-load-error-reported ()
  "A config that signals on load returns ok=:json-false with the message, and
leaves the connection usable."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--config-host _conn
    (let ((bad (make-temp-file "angelia-badcfg-" nil ".el"
                               "(error \"boom from config\")\n")))
      (unwind-protect
          (let ((resp (angelia-client-load-server-config
                       angelia-tests--config-host bad)))
            (should-not (eq (plist-get resp :ok) t))
            (should (string-match-p "boom from config" (plist-get resp :error)))
            ;; Server survived the bad load.
            (should (eq (plist-get (angelia-client-call
                                    angelia-tests--config-host 'server/ping nil)
                                   :pong)
                        t)))
        (when (file-exists-p bad) (delete-file bad))))))

(ert-deftest test-config-auto-load-survives-reconnect ()
  "With auto-load on, the config is applied on connect and re-applied after a
dropped connection comes back."
  (angelia-tests-ensure-no-connections)
  (angelia-tests--with-config-file cfg
    (let ((angelia-server-config-file cfg)
          (angelia-server-auto-load-config t)
          (angelia-client-auto-reconnect t)
          (angelia-client-reconnect-max-attempts 0))
      (unwind-protect
          (progn
            ;; Connect -> after-connect hook auto-loads the config.
            (angelia-client-connect angelia-tests--config-host)
            (should (= 42 (plist-get (angelia-tests--call
                                      angelia-tests--config-host 'custom/double "n" 21)
                                     :value)))
            ;; Kill the link; the next call reconnects and re-applies the config.
            (delete-process
             (angelia-client--conn-process
              (angelia-client--existing-live-connection
               angelia-tests--config-host)))
            (should (= 42 (plist-get (angelia-tests--call
                                      angelia-tests--config-host 'custom/double "n" 21)
                                     :value))))
        (when (gethash angelia-tests--config-host angelia-client--connections)
          (angelia-client-disconnect angelia-tests--config-host))))))

;;; test-config.el ends here
