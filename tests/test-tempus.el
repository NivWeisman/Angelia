;;; -*- lexical-binding: t; -*-
;;; test-tempus.el --- Layer 0 unit tests for tempus.el

;; Pure unit tests for the timing utility: no SSH, no network.  Each test binds
;; `tempus-log-function' to a local recorder so the real client/server loggers
;; are never touched, and toggles `tempus-debug' to exercise the gate.

(require 'ert)
(require 'tempus)

(ert-deftest test-tempus-returns-body-value ()
  "`tempus-measure' returns the value of its body (debug off)."
  (let ((tempus-debug nil)
        (tempus-log-function #'ignore))
    (should (= 42 (tempus-measure "add" (+ 40 2))))
    (should (equal '(a b) (tempus-measure "list" (list 'a 'b))))))

(ert-deftest test-tempus-body-evaluated-once ()
  "The body runs exactly once."
  (let ((tempus-debug t)
        (tempus-log-function #'ignore)
        (n 0))
    (tempus-measure "once" (setq n (1+ n)))
    (should (= n 1))))

(ert-deftest test-tempus-no-log-when-disabled ()
  "With `tempus-debug' nil, nothing is logged."
  (let* ((calls 0)
         (tempus-debug nil)
         (tempus-log-function (lambda (&rest _) (setq calls (1+ calls)))))
    (tempus-measure "quiet" (+ 1 1))
    (should (= calls 0))))

(ert-deftest test-tempus-logs-when-enabled ()
  "With `tempus-debug' non-nil, one timing line is logged with the label + ms fmt."
  (let* ((records nil)
         (tempus-debug t)
         (tempus-log-function (lambda (fmt &rest args)
                                (push (apply #'format fmt args) records))))
    (tempus-measure "the-label" (+ 1 1))
    (should (= 1 (length records)))
    (let ((line (car records)))
      (should (string-match-p "tempus the-label" line))
      (should (string-match-p "ms\\'" line)))))

(ert-deftest test-tempus-logs-on-error-path ()
  "Timing is logged even when the body signals an error (via `unwind-protect')."
  (let* ((calls 0)
         (tempus-debug t)
         (tempus-log-function (lambda (&rest _) (setq calls (1+ calls)))))
    (should-error (tempus-measure "boom" (error "kaboom")))
    (should (= calls 1))))

(ert-deftest test-tempus-label-evaluated-once ()
  "The LABEL expression is evaluated exactly once, even with logging on."
  (let* ((label-evals 0)
         (tempus-debug t)
         (tempus-log-function #'ignore))
    (tempus-measure (progn (setq label-evals (1+ label-evals)) "L")
      (+ 1 1))
    (should (= label-evals 1))))

(ert-deftest test-tempus-log-since ()
  "`tempus-log-since' logs once when enabled and stays silent when disabled."
  (let ((calls 0)
        (start (current-time)))
    (let ((tempus-debug nil)
          (tempus-log-function (lambda (&rest _) (setq calls (1+ calls)))))
      (tempus-log-since "x" start)
      (should (= calls 0)))
    (let ((tempus-debug t)
          (tempus-log-function (lambda (&rest _) (setq calls (1+ calls)))))
      (tempus-log-since "x" start)
      (should (= calls 1)))))

;;; test-tempus.el ends here
