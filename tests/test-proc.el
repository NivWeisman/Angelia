;;; -*- lexical-binding: t; -*-
;;; test-proc.el --- Layer 2 PTY tests (proc/start, input, resize, signal)

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-proc)

(defconst angelia-tests--proc-host "localhost"
  "Single-VM test target for PTY tests.")

(defmacro angelia-tests--with-proc (binds &rest body)
  "Connect, spawn (BINDS = ((HANDLE-VAR start-args COLLECTOR-VARS) ...)), run BODY,
clean up.  Each entry binds HANDLE-VAR to a freshly-started proc; collector
variables are bound to fresh containers around BODY.  See uses below for
the typical shape."
  (declare (indent 1))
  `(progn
     (angelia-tests-ensure-no-connections)
     (with-angelia-connection angelia-tests--proc-host _conn
       ,@body)))

(defun angelia-tests--proc-wait-for (predicate timeout &optional desc)
  "Pump events until PREDICATE returns non-nil or TIMEOUT elapses."
  (with-timeout (timeout (error "timed out: %s" (or desc "wait")))
    (while (not (funcall predicate))
      (accept-process-output nil 0.05))))

;; ---------------------------------------------------------------------------

(ert-deftest test-proc-echo ()
  "Spawn `echo HELLO PTY', collect output until exit, assert exit code 0."
  (angelia-tests--with-proc nil
    (let* ((output "")
           (exit-info nil)
           (handle (angelia-client-proc-start
                    angelia-tests--proc-host
                    (list "echo" "HELLO PTY")
                    :on-output (lambda (b) (setq output (concat output b)))
                    :on-exit   (lambda (i) (setq exit-info i)))))
      (angelia-tests--proc-wait-for (lambda () exit-info) 5 "exit")
      (should (string-match-p "HELLO PTY" output))
      (should (equal (plist-get exit-info :code) 0))
      (should (angelia-client-proc-exited handle)))))

(ert-deftest test-proc-cat-roundtrip ()
  "Start cat, send a line, see it echoed back through the PTY, then SIGTERM."
  (angelia-tests--with-proc nil
    (let* ((output "")
           (exit-info nil)
           (handle (angelia-client-proc-start
                    angelia-tests--proc-host
                    (list "cat")
                    :on-output (lambda (b) (setq output (concat output b)))
                    :on-exit   (lambda (i) (setq exit-info i)))))
      (angelia-client-proc-send handle "ping-back\n")
      (angelia-tests--proc-wait-for
       (lambda () (string-match-p "ping-back" output)) 5 "cat echo")
      (angelia-client-proc-signal handle "TERM")
      (angelia-tests--proc-wait-for (lambda () exit-info) 5 "cat exit")
      ;; PTY's onlcr usually doubles the line ("ping-back" once for the
      ;; tty echo, once for cat's stdout), but we only need to see it.
      (should (string-match-p "ping-back" output))
      (should (or (equal (plist-get exit-info :signal) "TERM")
                  ;; Some kernels report "signal 15" -- accept either.
                  (string-match-p "TERM\\|signal 15" (or (plist-get exit-info :event) "")))))))

(ert-deftest test-proc-signal ()
  "Spawn `sleep 30', send SIGTERM, verify exit-signal reports TERM."
  (angelia-tests--with-proc nil
    (let* ((exit-info nil)
           (handle (angelia-client-proc-start
                    angelia-tests--proc-host
                    (list "sleep" "30")
                    :on-exit (lambda (i) (setq exit-info i)))))
      ;; Tiny pause so sleep has actually started before we send the signal.
      (sleep-for 0.1)
      (angelia-client-proc-signal handle "TERM")
      (angelia-tests--proc-wait-for (lambda () exit-info) 3 "sleep exit")
      (should (or (equal (plist-get exit-info :signal) "TERM")
                  (string-match-p "TERM\\|signal 15"
                                  (or (plist-get exit-info :event) "")))))))

(ert-deftest test-proc-resize-at-start ()
  "Spawn `stty size' with rows=24 cols=80; assert output reports `24 80'."
  (angelia-tests--with-proc nil
    (let* ((output "")
           (exit-info nil)
           (handle (angelia-client-proc-start
                    angelia-tests--proc-host
                    (list "stty" "size")
                    :rows 24 :cols 80
                    :on-output (lambda (b) (setq output (concat output b)))
                    :on-exit   (lambda (i) (setq exit-info i)))))
      (angelia-tests--proc-wait-for (lambda () exit-info) 5 "stty exit")
      (should (string-match-p "24 80" output))
      (should (equal (plist-get exit-info :code) 0)))))

(ert-deftest test-proc-concurrent ()
  "Three concurrent echo procs deliver their own distinct outputs."
  (angelia-tests--with-proc nil
    (let* ((outs (make-vector 3 ""))
           (exits (make-vector 3 nil)))
      (dotimes (k 3)
        (let ((slot k))
          (angelia-client-proc-start
           angelia-tests--proc-host
           (list "echo" (format "marker-%d" slot))
           :on-output (lambda (b)
                        (aset outs slot
                              (concat (aref outs slot) b)))
           :on-exit (lambda (i) (aset exits slot i)))))
      (angelia-tests--proc-wait-for
       (lambda () (and (aref exits 0) (aref exits 1) (aref exits 2)))
       5 "three exits")
      (dotimes (k 3)
        (should (string-match-p (format "marker-%d" k) (aref outs k)))
        (should (equal (plist-get (aref exits k) :code) 0))))))

(ert-deftest test-proc-cleanup ()
  "Abandoning a session via close-session terminates the remote process.
We verify by `kill -0 PID' on the local fs (since the target is
ssh localhost, the remote PID == local PID).  A non-zero exit from
`kill -0' means the process is gone."
  (angelia-tests--with-proc nil
    (let* ((handle (angelia-client-proc-start
                    angelia-tests--proc-host
                    (list "sleep" "120")))
           (pid (angelia-client-proc-pid handle)))
      (should (integerp pid))
      ;; Process should be alive right after spawn.
      (should (zerop (call-process "kill" nil nil nil
                                   "-0" (number-to-string pid))))
      (angelia-client-proc-close handle)
      ;; Give the server's cleanup hook a moment to kill the child.
      (angelia-tests--proc-wait-for
       (lambda ()
         (not (zerop (call-process "kill" nil nil nil
                                   "-0" (number-to-string pid)))))
       5 "process to disappear")
      (should-not (zerop (call-process "kill" nil nil nil
                                       "-0" (number-to-string pid)))))))

;;; test-proc.el ends here
