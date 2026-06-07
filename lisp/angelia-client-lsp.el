;;; -*- lexical-binding: t; -*-
;;; angelia-client-lsp.el --- LSP support for Angelia remote hosts

;; Provides eglot and lsp-mode integration so that language servers running
;; on a remote host can serve buffers visiting `/@angelia:HOST:/path' files.
;;
;; Each language server is a direct SSH stdio subprocess:
;;   ssh HOST <login-wrapped lsp-command>
;; The remote command is built with `angelia-client--login-wrap' -- the same
;; wrapper Angelia uses for the server launch and every `ssh-run' -- so the
;; remote login environment (PATH, etc.) is loaded the way the host's actual
;; login shell loads it: csh/tcsh source ~/.login, zsh source ~/.zprofile, and
;; only sh/bash fall back to `bash --login'.  A hardcoded `bash --login -c'
;; (CLAUDE.md rule 5) silently fails to find the language server on PATH on
;; zsh/csh hosts -- which `brew shellenv' targets on modern macOS.
;; The language server's stdin/stdout carry Content-Length-framed LSP messages
;; directly; no Angelia JSON-RPC involvement in the data path.
;;
;; Usage example (init.el):
;;   (require 'angelia-client-lsp)
;;   (setq angelia-client-lsp-server-programs
;;         '((("user@remotehost" python-mode) . "pylsp")
;;           (("user@remotehost" rust-mode)   . "rust-analyzer")))
;;   (angelia-client-lsp-configure)

(require 'cl-lib)
(require 'subr-x)
(require 'angelia-client-files)
;; For `angelia-client--login-wrap': the language server launch must load the
;; remote login env the host's own shell way, not via a hardcoded `bash --login'.
(require 'angelia-client-deploy)

;; ---------------------------------------------------------------------------
;; URI mapping.
;;
;; Angelia path:  /@angelia:HOST:/remote/path
;; LSP URI:       file:///remote/path
;;
;; `angelia-client-files--parse' returns (HOST . REMOTE-PATH) where
;; REMOTE-PATH already has its leading slash, e.g. "/src/main.rs".
;; `angelia-client-files--make-path' reverses that.

(defun angelia-client-lsp--path-to-uri (path)
  "Convert /@angelia:HOST:/remote/path to file:///remote/path.
Returns nil when PATH is not an Angelia path (callers should fall through)."
  (when-let ((pair (angelia-client-files--parse path)))
    (concat "file://" (cdr pair))))

(defun angelia-client-lsp--uri-to-path (host uri)
  "Convert a file:///remote/path URI to /@angelia:HOST:/remote/path.
Returns nil when URI does not start with \"file://\" (callers should fall through)."
  (when (string-prefix-p "file://" uri)
    ;; Drop "file://" (7 chars) to get "/remote/path".
    (angelia-client-files--make-path host (substring uri 7))))

;; ---------------------------------------------------------------------------
;; Process spawning.

(defun angelia-client-lsp--make-process (host lsp-command)
  "Spawn LSP-COMMAND on HOST via SSH and return the live process object.
The remote command is built with `angelia-client--login-wrap', so HOST's own
login shell loads its login environment (PATH, etc.) before exec-ing the
language server -- the same path Angelia uses everywhere else.  LSP-COMMAND
must contain no single quotes (a csh login-wrap constraint)."
  (angelia-client--log "lsp: spawn host=%s cmd=%s" host lsp-command)
  (make-process
   :name            (format "angelia-lsp<%s>" host)
   :command         (list angelia-client-ssh-program host
                          (angelia-client--login-wrap host lsp-command))
   :connection-type 'pipe
   :coding          'binary
   :noquery         t))

;; ---------------------------------------------------------------------------
;; Per-host process registry.
;;
;; Tracks live eglot LSP subprocesses per host so `angelia-client--on-shutdown'
;; can kill them when the parent Angelia connection drops.

(defvar angelia-client-lsp--processes (make-hash-table :test #'equal)
  "Hash-table: HOST string → list of live LSP subprocess process objects.")

(defun angelia-client-lsp--register-process (host proc)
  "Track PROC as an LSP subprocess owned by the Angelia connection to HOST.
A sentinel is attached that removes PROC from the registry when it exits."
  (let ((prior (gethash host angelia-client-lsp--processes))
        (existing-sentinel (process-sentinel proc)))
    (puthash host (cons proc prior) angelia-client-lsp--processes)
    (set-process-sentinel
     proc
     (lambda (p event)
       (angelia-client-lsp--deregister-process host p)
       (when (functionp existing-sentinel)
         (funcall existing-sentinel p event))))))

(defun angelia-client-lsp--deregister-process (host proc)
  "Remove PROC from the registry for HOST."
  (let ((updated (delq proc (gethash host angelia-client-lsp--processes))))
    (if updated
        (puthash host updated angelia-client-lsp--processes)
      (remhash host angelia-client-lsp--processes))))

(defun angelia-client-lsp--cleanup-host (host)
  "Kill every live LSP subprocess registered under HOST."
  (dolist (proc (gethash host angelia-client-lsp--processes))
    (when (process-live-p proc)
      (angelia-client--log "lsp: killing %s on Angelia disconnect of %s"
                           (process-name proc) host)
      (delete-process proc)))
  (remhash host angelia-client-lsp--processes))

;; ---------------------------------------------------------------------------
;; Eglot integration.

(with-eval-after-load 'eglot
  (require 'eieio)

  (defclass angelia-lsp-server (eglot-lsp-server)
    ((angelia-host
      :initarg     :angelia-host
      :accessor    angelia-lsp-server-host
      :documentation "The Angelia SSH host string for this language server."))
    :documentation
    "Eglot LSP server subclass for language servers on Angelia remote hosts.")

  ;; URI advice — `:around' wrappers that are no-ops for non-Angelia paths,
  ;; so they are safe to install globally.

  (defun angelia-client-lsp--eglot-path-to-uri (orig path)
    "Advice for `eglot--path-to-uri': translate /@angelia: paths to file:// URIs."
    (or (angelia-client-lsp--path-to-uri path) (funcall orig path)))

  (defun angelia-client-lsp--eglot-uri-to-path (orig uri)
    "Advice for `eglot--uri-to-path': restore /@angelia: prefix for file:// URIs."
    (let* ((server (ignore-errors
                     (and (fboundp 'eglot-current-server) (eglot-current-server))))
           (host   (and server
                        (eieio-object-p server)
                        (object-of-class-p server 'angelia-lsp-server)
                        (slot-value server 'angelia-host)))
           (result (and host (angelia-client-lsp--uri-to-path host uri))))
      (or result (funcall orig uri))))

  (advice-add 'eglot--path-to-uri :around #'angelia-client-lsp--eglot-path-to-uri)
  (advice-add 'eglot--uri-to-path :around #'angelia-client-lsp--eglot-uri-to-path)

  (defun angelia-client-lsp-eglot-setup (host mode lsp-command)
    "Add an `eglot-server-programs' entry for MODE on HOST using LSP-COMMAND.
The entry uses `angelia-lsp-server' so that URI advice can recover the HOST
when translating file:// URIs back to /@angelia: paths."
    (add-to-list 'eglot-server-programs
                 `(,mode angelia-lsp-server
                         :angelia-host ,host
                         :process      ,(let ((h host) (c lsp-command))
                                          (lambda ()
                                            (let ((proc (angelia-client-lsp--make-process h c)))
                                              (angelia-client-lsp--register-process h proc)
                                              proc)))))))

;; ---------------------------------------------------------------------------
;; lsp-mode integration.
;;
;; lsp-mode accepts per-client `:path-to-uri' and `:uri-to-path' closures, so
;; no global advice is needed.  The connection is an lsp-stdio-connection whose
;; command is the same SSH invocation used by the eglot path.  lsp-mode manages
;; the subprocess lifecycle; when the SSH pipe dies the process sentinel fires
;; and lsp-mode cleans up automatically.

(with-eval-after-load 'lsp-mode
  (defun angelia-client-lsp-lsp-mode-setup (host mode lsp-command)
    "Register an lsp-mode client for MODE on HOST using LSP-COMMAND."
    (let ((server-id (intern (format "angelia-lsp-%s-%s" host mode))))
      (lsp-register-client
       (make-lsp-client
        :server-id      server-id
        :new-connection (lsp-stdio-connection
                         (let ((h host) (c lsp-command))
                           (lambda ()
                             (list angelia-client-ssh-program h
                                   (angelia-client--login-wrap h c)))))
        :major-modes    (list mode)
        :priority       10
        :activation-fn  (let ((h host))
                          (lambda (file-name _mode)
                            (string-prefix-p
                             (format "/@angelia:%s:" h) file-name)))
        :path-to-uri    #'angelia-client-lsp--path-to-uri
        :uri-to-path    (let ((h host))
                          (lambda (uri)
                            (or (angelia-client-lsp--uri-to-path h uri) uri))))))))

;; ---------------------------------------------------------------------------
;; Top-level configuration variable and helper.

(defvar angelia-client-lsp-server-programs nil
  "Alist of ((HOST MODE) . \"lsp-command\") entries for remote LSP servers.
HOST is an SSH destination string (same as passed to `angelia-client-connect').
MODE is a major-mode symbol.  \"lsp-command\" is the language server command
string as it would be typed on the remote host's shell.

Call `angelia-client-lsp-configure' after setting this variable.  Example:

  (setq angelia-client-lsp-server-programs
        \\='(((\"user@remotehost\" python-mode) . \"pylsp\")
          ((\"user@remotehost\" rust-mode)   . \"rust-analyzer\")))
  (angelia-client-lsp-configure)")

(defun angelia-client-lsp-configure ()
  "Register all entries in `angelia-client-lsp-server-programs' with eglot and lsp-mode.
Safe to call multiple times; defers registration until the relevant package loads."
  (pcase-dolist (`((,host ,mode) . ,cmd) angelia-client-lsp-server-programs)
    (with-eval-after-load 'eglot
      (angelia-client-lsp-eglot-setup host mode cmd))
    (with-eval-after-load 'lsp-mode
      (angelia-client-lsp-lsp-mode-setup host mode cmd))))

;;;###autoload
(defun angelia-client-lsp-configure-from-server (host)
  "Configure LSP for HOST from the programs its server config declared.
Queries `server/lsp-programs' (populated by `angelia-server-register-lsp' in the
injected config) and registers each MODE -> COMMAND for this host via the same
machinery as `angelia-client-lsp-server-programs'.  The LSP processes still
launch client-side over ssh; this only carries the per-host *policy*."
  (interactive (list (angelia-client--read-host "Configure LSP from host: ")))
  (let* ((resp (angelia-client-call host 'server/lsp-programs nil))
         (programs (plist-get resp :programs))
         (added 0))
    ;; jsonrpc decodes the JSON object as a plist with keyword keys, e.g.
    ;; (:python-mode "pylsp" :rust-mode "rust-analyzer").
    (while programs
      (let* ((mode (intern (substring (symbol-name (car programs)) 1)))
             (cmd (cadr programs)))
        (when (stringp cmd)
          (setf (alist-get (list host mode) angelia-client-lsp-server-programs
                           nil nil #'equal)
                cmd)
          (cl-incf added))
        (setq programs (cddr programs))))
    (when (> added 0) (angelia-client-lsp-configure))
    (angelia-client--log "lsp: configured %d program(s) from %s" added host)
    added))

(provide 'angelia-client-lsp)
;;; angelia-client-lsp.el ends here
