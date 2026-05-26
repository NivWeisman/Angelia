;;; -*- lexical-binding: t; -*-
;;; angelia-client-proc.el --- Remote PTY processes over Angelia

;; Client surface for `proc/start' & friends.  A process handle wraps the
;; server-issued session id and the ON-OUTPUT / ON-EXIT callbacks registered
;; against it; everything else (send, resize, signal, close) becomes a thin
;; RPC wrapper.
;;
;; A minimal `angelia-client-term-mode' buffer adapter is included so
;; `M-x angelia-client-term' works for interactive smoke-testing.  It is
;; deliberately small -- output is appended raw (no ANSI emulation), keys
;; are forwarded one-event-at-a-time.  Users who want full PTY behaviour
;; (colors, cursor addressing, alternate screens, arrow keys for line
;; editing) can write their own adapter on top of the API below.

(require 'cl-lib)
(require 'subr-x)
(require 'angelia-client)

;; ---------------------------------------------------------------------------
;; Process handle.

(cl-defstruct (angelia-client-proc
               (:constructor angelia-client-proc--create))
  "Bookkeeping for one remote process session."
  host session pid on-output on-exit buffer
  (exited nil))

(defun angelia-client-proc-live-p (handle)
  "Return non-nil if HANDLE's remote process hasn't reported `exit' yet."
  (and (angelia-client-proc-p handle)
       (not (angelia-client-proc-exited handle))))

;; ---------------------------------------------------------------------------
;; Lifecycle.

(defcustom angelia-client-default-persistence-backend 'dtach
  "Default backend symbol used when `angelia-client-proc-start' is called
with `:persist' but no `:backend'.  One of `dtach', `tmux', `screen'."
  :type '(choice (const dtach) (const tmux) (const screen))
  :group 'angelia)

(defun angelia-client-proc--backend-string (backend)
  "Normalise BACKEND (symbol or string) to its wire-format string form."
  (cond ((null backend) nil)
        ((symbolp backend) (symbol-name backend))
        ((stringp backend) backend)
        (t (error "angelia: unknown backend value %S" backend))))

(cl-defun angelia-client-proc-start (host argv
                                          &key cwd env rows cols
                                          persist backend
                                          on-output on-exit buffer)
  "Spawn a PTY-backed process on HOST running ARGV; return a handle.
ON-OUTPUT, when non-nil, is called with each chunk of bytes received
from the remote PTY (as a unibyte string).  ON-EXIT, when non-nil, is
called once with `(:code N|nil :signal STR|nil :event STR)' on
termination.  CWD / ENV / ROWS / COLS are optional spawn parameters.

PERSIST, when set, names a persisted session that survives the SSH
connection: the remote spawns the process under a wrapper backend
(dtach/tmux/screen).  BACKEND selects which wrapper -- defaults to
`angelia-client-default-persistence-backend' (`dtach' out of the box).
When PERSIST is nil, BACKEND is ignored."
  (let* ((conn (angelia-client-connection host))
         (params (make-hash-table :test #'equal)))
    (puthash "argv" (vconcat argv) params)
    (when cwd  (puthash "cwd"  cwd  params))
    (when env
      (let ((h (make-hash-table :test #'equal)))
        (cl-loop for (k v) on env by #'cddr
                 do (puthash (if (keywordp k) (substring (symbol-name k) 1) k)
                             v h))
        (puthash "env" h params)))
    (when rows (puthash "rows" rows params))
    (when cols (puthash "cols" cols params))
    (when persist
      (puthash "persist" persist params)
      (puthash "backend"
               (angelia-client-proc--backend-string
                (or backend angelia-client-default-persistence-backend))
               params))
    (let* ((result (jsonrpc-request (angelia-client--conn-jsonrpc conn)
                                    'proc/start params))
           (session (plist-get result :session))
           (pid (plist-get result :pid)))
      (unless (stringp session)
        (signal 'angelia-client-session-error
                (list :host host :method 'proc/start :result result)))
      (let ((handle (angelia-client-proc--create
                     :host host :session session :pid pid
                     :on-output on-output :on-exit on-exit
                     :buffer buffer)))
        (puthash session
                 (list :on-event
                       (lambda (kind p)
                         (pcase kind
                           ("output"
                            (when (angelia-client-proc-on-output handle)
                              (condition-case err
                                  (funcall (angelia-client-proc-on-output handle)
                                           (base64-decode-string
                                            (plist-get p :data)))
                                (error
                                 (angelia-client--log-error
                                  (format "proc on-output session=%s" session)
                                  err)))))
                           ("exit"
                            (setf (angelia-client-proc-exited handle) t)
                            (when (angelia-client-proc-on-exit handle)
                              (condition-case err
                                  (funcall (angelia-client-proc-on-exit handle)
                                           (list :code (plist-get p :code)
                                                 :signal (plist-get p :signal)
                                                 :event (plist-get p :event)))
                                (error
                                 (angelia-client--log-error
                                  (format "proc on-exit session=%s" session)
                                  err)))))))
                       :on-end
                       (lambda (_p)
                         ;; Defensive: if `exit' never ran (transport drop
                         ;; with a close-on-our-side), still mark dead.
                         (setf (angelia-client-proc-exited handle) t)))
                 (angelia-client--conn-sessions conn))
        (angelia-client--log
         "proc started: host=%s session=%s pid=%s argv=%S"
         host session pid argv)
        handle))))

(defun angelia-client-proc--params-for (handle)
  "Return a fresh params hash carrying just the session id from HANDLE."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" (angelia-client-proc-session handle) p)
    p))

(defun angelia-client-proc-send (handle data)
  "Send DATA (a string of bytes) to the PTY stdin of HANDLE.
DATA may be unibyte or multibyte; multibyte is encoded as UTF-8."
  (let ((bytes (if (multibyte-string-p data)
                   (encode-coding-string data 'utf-8 t)
                 data))
        (p (angelia-client-proc--params-for handle)))
    (puthash "data" (base64-encode-string bytes t) p)
    (angelia-client-call (angelia-client-proc-host handle) 'proc/input p)))

(defun angelia-client-proc-resize (handle rows cols)
  "Resize the remote PTY for HANDLE to ROWS by COLS."
  (let ((p (angelia-client-proc--params-for handle)))
    (puthash "rows" rows p)
    (puthash "cols" cols p)
    (angelia-client-call (angelia-client-proc-host handle) 'proc/resize p)))

(defun angelia-client-proc-signal (handle signal)
  "Send SIGNAL (string like \"TERM\") to the remote process of HANDLE."
  (let ((p (angelia-client-proc--params-for handle)))
    (puthash "signal" signal p)
    (angelia-client-call (angelia-client-proc-host handle) 'proc/signal p)))

(defun angelia-client-proc-close (handle)
  "Close HANDLE's session.  Kills the remote process via session cleanup."
  (let ((host (angelia-client-proc-host handle))
        (session (angelia-client-proc-session handle)))
    (when (and host session)
      (angelia-client-close-session host session))
    (setf (angelia-client-proc-exited handle) t)))

;; ---------------------------------------------------------------------------
;; Minimal term-mode-like buffer adapter.
;;
;; Output is appended raw (no ANSI emulation, no cursor addressing).  Each
;; self-inserting key sends its character.  RET sends "\n"; backspace sends
;; `\x7f' (DEL).  C-c is bound to send SIGINT; C-d sends EOT.

(defvar-local angelia-client-term--handle nil
  "The `angelia-client-proc' handle backing the current term buffer.")

(defun angelia-client-term--send-keys ()
  "Forward `this-command-keys' to the buffer's `angelia-client-proc' handle."
  (interactive)
  (when angelia-client-term--handle
    (let* ((keys (this-command-keys))
           (str (cond
                 ((stringp keys) keys)
                 ((vectorp keys)
                  (mapconcat (lambda (k)
                               (cond ((characterp k) (string k))
                                     ((eq k 'return) "\r")
                                     ((eq k 'tab) "\t")
                                     ((eq k 'backspace) "\x7f")
                                     (t "")))
                             keys ""))
                 (t ""))))
      (when (> (length str) 0)
        (angelia-client-proc-send angelia-client-term--handle str)))))

(defun angelia-client-term-send-interrupt ()
  "Send SIGINT to the term buffer's remote process."
  (interactive)
  (when angelia-client-term--handle
    (angelia-client-proc-signal angelia-client-term--handle "INT")))

(defvar angelia-client-term-mode-map
  (let ((m (make-keymap)))
    (suppress-keymap m)
    (dotimes (k 128)
      (when (>= k 32)                            ; printable ASCII
        (define-key m (vector k) #'angelia-client-term--send-keys)))
    (define-key m (kbd "RET") #'angelia-client-term--send-keys)
    (define-key m (kbd "TAB") #'angelia-client-term--send-keys)
    (define-key m (kbd "DEL") #'angelia-client-term--send-keys)
    (define-key m (kbd "C-c C-c") #'angelia-client-term-send-interrupt)
    (define-key m (kbd "C-c C-d") (lambda () (interactive)
                                    (angelia-client-proc-send
                                     angelia-client-term--handle "\x04")))
    m)
  "Keymap for `angelia-client-term-mode'.")

(define-derived-mode angelia-client-term-mode fundamental-mode "AngeliaTerm"
  "Minimal append-only terminal-like buffer backed by a remote angelia process.
No ANSI emulation; users who want a real terminal should write a richer
adapter on top of `angelia-client-proc-start'."
  (setq buffer-read-only nil
        truncate-lines t))

;;;###autoload
(defun angelia-client-term (host argv)
  "Open an `angelia-client-term-mode' buffer running ARGV on HOST.
ARGV is a list of strings (e.g. `(\"bash\")' or `(\"top\" \"-n\" \"1\")')."
  (interactive
   (list (read-string "Host: " "localhost")
         (split-string-and-unquote (read-string "Command: " "bash"))))
  (let* ((buf (generate-new-buffer
               (format "*angelia-term:%s:%s*" host (car argv))))
         handle)
    (with-current-buffer buf (angelia-client-term-mode))
    (setq handle
          (angelia-client-proc-start
           host argv
           :on-output
           (lambda (bytes)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-max))
                   (let ((inhibit-read-only t))
                     (insert (decode-coding-string bytes 'utf-8 t)))))))
           :on-exit
           (lambda (info)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (format
                            "\n[angelia: process exited code=%S signal=%S]\n"
                            (plist-get info :code)
                            (plist-get info :signal)))))))))
    (with-current-buffer buf
      (setq-local angelia-client-term--handle handle))
    (switch-to-buffer buf)
    handle))

;; ---------------------------------------------------------------------------
;; Persistence: list / reattach / detach / kill.

(defun angelia-client-proc-list-persisted (host &optional backend)
  "Return the persisted sessions on HOST as a list of plists.
Each element is `(:name STR :backend STR :alive BOOL)'.  When BACKEND is
non-nil, the server restricts the listing to that backend."
  (let ((p (make-hash-table :test #'equal)))
    (when backend
      (puthash "backend"
               (angelia-client-proc--backend-string backend) p))
    (let* ((result (angelia-client-call host 'proc/list-persisted p))
           (sessions (plist-get result :sessions)))
      (mapcar (lambda (s)
                (list :name (plist-get s :name)
                      :backend (plist-get s :backend)
                      :alive (eq (plist-get s :alive) t)))
              (append sessions nil)))))

(cl-defun angelia-client-proc-reattach (host name backend
                                             &key on-output on-exit buffer)
  "Open a fresh PTY session re-entering persisted NAME under BACKEND on HOST.
Callbacks behave exactly as `angelia-client-proc-start'.  Returns a handle."
  (let* ((conn (angelia-client-connection host))
         (params (make-hash-table :test #'equal)))
    (puthash "name" name params)
    (puthash "backend" (angelia-client-proc--backend-string backend) params)
    (let* ((result (jsonrpc-request (angelia-client--conn-jsonrpc conn)
                                    'proc/reattach params))
           (session (plist-get result :session))
           (pid (plist-get result :pid)))
      (unless (stringp session)
        (signal 'angelia-client-session-error
                (list :host host :method 'proc/reattach :result result)))
      (let ((handle (angelia-client-proc--create
                     :host host :session session :pid pid
                     :on-output on-output :on-exit on-exit
                     :buffer buffer)))
        (puthash session
                 (list :on-event
                       (lambda (kind p)
                         (pcase kind
                           ("output"
                            (when (angelia-client-proc-on-output handle)
                              (condition-case err
                                  (funcall (angelia-client-proc-on-output handle)
                                           (base64-decode-string
                                            (plist-get p :data)))
                                (error
                                 (angelia-client--log-error
                                  (format "proc on-output session=%s" session)
                                  err)))))
                           ("exit"
                            (setf (angelia-client-proc-exited handle) t)
                            (when (angelia-client-proc-on-exit handle)
                              (condition-case err
                                  (funcall (angelia-client-proc-on-exit handle)
                                           (list :code (plist-get p :code)
                                                 :signal (plist-get p :signal)
                                                 :event (plist-get p :event)))
                                (error
                                 (angelia-client--log-error
                                  (format "proc on-exit session=%s" session)
                                  err)))))))
                       :on-end
                       (lambda (_p)
                         (setf (angelia-client-proc-exited handle) t)))
                 (angelia-client--conn-sessions conn))
        (angelia-client--log
         "proc reattached: host=%s session=%s pid=%s name=%s backend=%s"
         host session pid name backend)
        handle))))

(defun angelia-client-proc-detach (handle)
  "Close HANDLE's local PTY session but leave the persisted process running.
Functionally identical to `angelia-client-proc-close' for non-persisted
processes (i.e. it ends the wrapped connector, and the backend keeps the
underlying CMD alive only when there is one)."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" (angelia-client-proc-session handle) p)
    (when-let ((conn (gethash (angelia-client-proc-host handle)
                              angelia-client--connections)))
      (remhash (angelia-client-proc-session handle)
               (angelia-client--conn-sessions conn)))
    (condition-case err
        (angelia-client-call (angelia-client-proc-host handle) 'proc/detach p 5)
      (error
       (angelia-client--log-error
        (format "proc/detach session=%s"
                (angelia-client-proc-session handle))
        err)))
    (setf (angelia-client-proc-exited handle) t)))

(defun angelia-client-proc-kill-persisted (host name backend)
  "Tear down persisted NAME on BACKEND on HOST via the backend's kill hook."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "name" name p)
    (puthash "backend" (angelia-client-proc--backend-string backend) p)
    (angelia-client-call host 'proc/kill-persisted p)
    nil))

;; ---------------------------------------------------------------------------
;; M-x angelia-client-list-persisted -- tabulated browser.

(defvar-local angelia-client--lp-host nil
  "Host the current `angelia-client-list-persisted' buffer was opened against.")

(defun angelia-client--lp-current-row ()
  "Return (BACKEND-STR NAME-STR) for the row at point, or nil."
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (cons (aref entry 0) (aref entry 1)))))

(defun angelia-client-list-persisted-reattach ()
  "Reattach to the persisted session at point.  Spawns an `angelia-client-term'
buffer for the new PTY."
  (interactive)
  (let* ((row (angelia-client--lp-current-row))
         (backend (car row))
         (name (cdr row))
         (host angelia-client--lp-host)
         (buf (generate-new-buffer
               (format "*angelia-term:%s:%s/%s*" host backend name))))
    (with-current-buffer buf (angelia-client-term-mode))
    (let ((handle (angelia-client-proc-reattach
                   host name (intern backend)
                   :on-output
                   (lambda (bytes)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char (point-max))
                           (let ((inhibit-read-only t))
                             (insert (decode-coding-string bytes 'utf-8 t)))))))
                   :on-exit
                   (lambda (info)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (let ((inhibit-read-only t))
                           (goto-char (point-max))
                           (insert
                            (format
                             "\n[angelia: reattached process exited code=%S signal=%S]\n"
                             (plist-get info :code)
                             (plist-get info :signal))))))))))
      (with-current-buffer buf
        (setq-local angelia-client-term--handle handle))
      (switch-to-buffer buf))))

(defun angelia-client-list-persisted-kill ()
  "Kill the persisted session at point via its backend's kill callback."
  (interactive)
  (let* ((row (angelia-client--lp-current-row))
         (backend (car row))
         (name (cdr row)))
    (when (yes-or-no-p (format "Kill persisted %s/%s on %s? "
                               backend name angelia-client--lp-host))
      (angelia-client-proc-kill-persisted
       angelia-client--lp-host name (intern backend))
      (angelia-client-list-persisted-refresh))))

(defun angelia-client-list-persisted-refresh ()
  "Re-fetch the persistence list and redraw the current buffer."
  (interactive)
  (when angelia-client--lp-host
    (let ((rows (mapcar (lambda (s)
                          (list nil
                                (vector (plist-get s :backend)
                                        (plist-get s :name)
                                        (if (plist-get s :alive)
                                            "alive" "dead"))))
                        (angelia-client-proc-list-persisted
                         angelia-client--lp-host))))
      (setq tabulated-list-entries rows)
      (tabulated-list-print t))))

(defvar angelia-client-list-persisted-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'angelia-client-list-persisted-reattach)
    (define-key m (kbd "d")   #'angelia-client-list-persisted-kill)
    (define-key m (kbd "g")   #'angelia-client-list-persisted-refresh)
    m)
  "Keymap for `angelia-client-list-persisted-mode'.")

(define-derived-mode angelia-client-list-persisted-mode tabulated-list-mode
  "Angelia-Persisted"
  "Browse persisted angelia sessions across all backends."
  (setq tabulated-list-format
        [("Backend" 10 t) ("Name" 24 t) ("Status" 8 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun angelia-client-list-persisted (host)
  "Open a buffer listing the persisted sessions on HOST across all backends.
Bindings: RET reattaches, `d' kills, `g' refreshes."
  (interactive (list (read-string "Host: " "localhost")))
  (let ((buf (get-buffer-create (format "*angelia-persisted:%s*" host))))
    (with-current-buffer buf
      (angelia-client-list-persisted-mode)
      (setq angelia-client--lp-host host)
      (angelia-client-list-persisted-refresh))
    (switch-to-buffer buf)))

(provide 'angelia-client-proc)
;;; angelia-client-proc.el ends here
