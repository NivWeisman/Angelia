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

(cl-defun angelia-client-proc-start (host argv
                                          &key cwd env rows cols
                                          on-output on-exit buffer)
  "Spawn a PTY-backed process on HOST running ARGV; return a handle.
ON-OUTPUT, when non-nil, is called with each chunk of bytes received
from the remote PTY (as a unibyte string).  ON-EXIT, when non-nil, is
called once with `(:code N|nil :signal STR|nil :event STR)' on
termination.  CWD / ENV / ROWS / COLS are optional spawn parameters."
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

(provide 'angelia-client-proc)
;;; angelia-client-proc.el ends here
