;;; -*- lexical-binding: t; -*-
;;; angelia-client-config.el --- Push a dedicated config file into the server

;; Optional feature: deploy a LOCAL elisp file (NOT your init.el) into the remote
;; Angelia server and load it there.  The server runs `--batch -Q', so this is
;; the one place to extend the I/O backend without inheriting the remote host's
;; own (possibly clunky) init: register custom RPC methods
;; (`angelia-server-register-method'), set up projects / `process-environment',
;; and declare which LSP servers the host offers (`angelia-server-register-lsp').
;;
;; The config must be batch-safe and must NOT write to stdout (that stream
;; carries JSON-RPC); the server guards stdout during the load regardless, but a
;; config that hangs waiting on a frame or signals on load is still on you.
;;
;; The server is fresh on every (re)connect, so the config is re-applied through
;; `angelia-client-after-connect-functions' -- including after an auto-reconnect.
;;
;; Enable by requiring this module and pointing `angelia-server-config-file' at
;; your file; loading is automatic on connect unless `angelia-server-auto-load-
;; config' is nil, in which case use `M-x angelia-client-load-server-config'.

(require 'cl-lib)
(require 'subr-x)
(require 'angelia-client)

(defcustom angelia-server-config-file nil
  "Local elisp file loaded into the remote Angelia server after connecting.
Runs in the server's `--batch -Q' process.  nil disables the feature.  See this
module's commentary for what belongs here (and what must not -- stdout writes)."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'angelia)

(defcustom angelia-server-auto-load-config t
  "When non-nil, load `angelia-server-config-file' automatically on (re)connect."
  :type 'boolean
  :group 'angelia)

(defun angelia-client-config--read-file (path)
  "Return the unibyte contents of PATH, or signal if it is unreadable."
  (unless (and (stringp path) (file-readable-p path))
    (error "angelia: server config file not readable: %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

;;;###autoload
(defun angelia-client-load-server-config (host &optional file)
  "Send FILE (default `angelia-server-config-file') to HOST's server and load it.
Reports the outcome and, when eglot/lsp-mode integration is present, configures
this host's LSP servers from the config's declarations.  Returns the server's
result plist."
  (interactive (list (angelia-client--read-host "Load server config on host: ")))
  (let* ((path (or file angelia-server-config-file))
         (content (angelia-client-config--read-file path))
         (params (make-hash-table :test #'equal)))
    (puthash "content" (base64-encode-string content t) params)
    (angelia-client--log "load-server-config: host=%s file=%s (%d bytes)"
                         host path (length content))
    (let ((resp (angelia-client-call host 'server/load-config params)))
      (if (eq (plist-get resp :ok) t)
          (progn
            (angelia-client--log "load-server-config ok host=%s methods=%S"
                                 host (plist-get resp :methods))
            ;; Apply any LSP declarations the config made (client launches them).
            (when (fboundp 'angelia-client-lsp-configure-from-server)
              (ignore-errors (angelia-client-lsp-configure-from-server host)))
            (when (called-interactively-p 'interactive)
              (message "angelia: loaded server config on %s" host)))
        (let ((err (plist-get resp :error)))
          (angelia-client--log "load-server-config FAILED host=%s: %s" host err)
          (when (called-interactively-p 'interactive)
            (message "angelia: server config failed on %s: %s" host err))))
      resp)))

(defun angelia-client-config--maybe-load (host)
  "Auto-load the server config on HOST when enabled.
Registered on `angelia-client-after-connect-functions', so it runs on the
initial connect and on every reconnect.  Errors are logged, never fatal."
  (when (and angelia-server-auto-load-config
             angelia-server-config-file)
    (condition-case err
        (angelia-client-load-server-config host)
      (error (angelia-client--log-error
              (format "auto load-server-config host=%s" host) err)))))

(add-hook 'angelia-client-after-connect-functions
          #'angelia-client-config--maybe-load)

(provide 'angelia-client-config)
;;; angelia-client-config.el ends here
