;;; -*- lexical-binding: t; -*-
;;; test-term.el --- Layer 2 tests for `angelia-client-term-mode'

;; Exercises the buffer adapter on top of `angelia-client-proc-start':
;; the major mode, the keymap-driven keystroke -> proc/input dispatch,
;; the on-output -> buffer-insert path, and the exit footer.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-proc)

(defconst angelia-tests--term-host "localhost"
  "Single-VM test target for term-adapter tests.")

(defmacro angelia-tests--with-term-buffer (handle-var buf-var argv &rest body)
  "Connect, open `angelia-client-term' for ARGV on the test host, run BODY.
HANDLE-VAR is bound to the resulting handle; BUF-VAR to its term buffer.
An unwind-protect closes the handle and kills the buffer."
  (declare (indent 3))
  `(progn
     (angelia-tests-ensure-no-connections)
     (with-angelia-connection angelia-tests--term-host _conn
       (let* ((,handle-var (angelia-client-term
                            angelia-tests--term-host ,argv))
              (,buf-var (angelia-client-proc-buffer ,handle-var)))
         (unwind-protect
             (progn ,@body)
           (ignore-errors (angelia-client-proc-close ,handle-var))
           (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var)))))))

(defun angelia-tests--term-wait-for (buf pattern &optional timeout desc)
  "Pump events until BUF's contents match PATTERN; signal on timeout."
  (with-timeout ((or timeout 5)
                 (error "timed out waiting for %S in %s; last buf=%S"
                        pattern (or desc "term buffer")
                        (and (buffer-live-p buf)
                             (with-current-buffer buf (buffer-string)))))
    (while (not (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (string-match-p pattern (buffer-string)))))
      (accept-process-output nil 0.05))))

;; ---------------------------------------------------------------------------

(ert-deftest test-term-spawn-and-mode-setup ()
  "`angelia-client-term' creates a buffer in the right mode with handle bound."
  (angelia-tests--with-term-buffer handle buf '("cat")
    (should (angelia-client-proc-p handle))
    (should (buffer-live-p buf))
    (with-current-buffer buf
      (should (eq major-mode 'angelia-client-term-mode))
      (should (eq angelia-client-term--handle handle))
      ;; A representative binding survives the mode setup.
      (should (eq (lookup-key angelia-client-term-mode-map (kbd "C-c C-c"))
                  'angelia-client-term-send-interrupt))
      ;; A printable-ASCII binding routes through `--send-keys'.
      (should (eq (lookup-key angelia-client-term-mode-map (kbd "a"))
                  'angelia-client-term--send-keys)))))

(ert-deftest test-term-output-appears-in-buffer ()
  "Output from the remote process is appended to the term buffer, and the
exit footer fires when the process exits cleanly."
  (angelia-tests--with-term-buffer handle buf
      '("bash" "--norc" "--noprofile" "-c" "echo HELLO_TERM_BUFFER")
    (angelia-tests--term-wait-for buf "HELLO_TERM_BUFFER" 5 "spawn output")
    (angelia-tests--term-wait-for buf "process exited" 5 "exit footer")
    ;; The code in the footer should be 0 (clean exit).
    (with-current-buffer buf
      (should (string-match-p "process exited code=0" (buffer-string))))))

(ert-deftest test-term-keystroke-roundtrip-and-interrupt ()
  "Simulated keystrokes traverse the keymap and reach the remote PTY; C-c C-c
sends SIGINT and the exit footer reports the signal."
  (angelia-tests--with-term-buffer handle buf '("cat")
    (with-current-buffer buf
      ;; cat is now reading from its PTY.  Type "ping" + Enter via the
      ;; mode's keymap.  Each event triggers `angelia-client-term--send-keys',
      ;; which reads `this-command-keys' and forwards bytes via proc/input.
      (execute-kbd-macro (kbd "p i n g RET")))
    ;; cat (PTY-echoed) prints the line back; wait for it in the buffer.
    (angelia-tests--term-wait-for buf "ping" 5 "cat echo")
    ;; C-c C-c invokes `angelia-client-term-send-interrupt'.
    (with-current-buffer buf
      (execute-kbd-macro (kbd "C-c C-c")))
    ;; Expect the exit footer reporting SIGINT.
    (angelia-tests--term-wait-for buf "process exited.*INT" 5
                                  "interrupt exit footer")))

;;; test-term.el ends here
