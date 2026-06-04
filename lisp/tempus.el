;;; -*- lexical-binding: t; -*-
;;; tempus.el --- Tiny debug-gated timing utility

;; Tempus measures how long a body of code takes and logs the elapsed time --
;; but only when debug mode is on.  It is a standalone, zero-dependency package
;; (own `tempus-' prefix) so it can be reused anywhere; Angelia wraps every RPC
;; task in `tempus-measure' to find bottlenecks.
;;
;; Each host installs its own logger via `tempus-log-function' and toggles
;; output via `tempus-debug'.  When debug is off the macro adds no logging --
;; it just evaluates the body and returns its value.

(defvar tempus-debug nil
  "When non-nil, `tempus-measure' logs elapsed timings; otherwise zero overhead.")

(defvar tempus-log-function #'ignore
  "Function emitting one timing line.
Called as (apply tempus-log-function FMT ARGS).  Each host installs its own
logger; both `angelia-client--log' and `angelia-server--log' already have the
matching (FMT &rest ARGS) signature.")

(defun tempus-log-since (label start)
  "Log the elapsed ms since START (a time value) under LABEL, if `tempus-debug'.
START is what `current-time' returned when the timed work began.  Use this for
work whose start and end straddle callbacks (e.g. async RPC); for a plain body
use `tempus-measure'."
  (when tempus-debug
    (apply tempus-log-function
           "tempus %s | %.1f ms"
           (list label
                 (* 1000 (float-time (time-subtract (current-time) start)))))))

(defmacro tempus-measure (label &rest body)
  "Evaluate BODY once and return its value.
When `tempus-debug' is non-nil, log the wall-clock time BODY took (in ms) via
`tempus-log-function', keyed by LABEL (formatted with %s).  Timing is recorded
even when BODY exits non-locally (error/throw), so a slow task that fails is
still visible to bottleneck hunts."
  (declare (indent 1) (debug (form body)))
  (let ((t0 (gensym "t0-"))
        (lbl (gensym "lbl-")))
    `(let ((,t0 (current-time))
           (,lbl ,label))
       (unwind-protect
           (progn ,@body)
         (tempus-log-since ,lbl ,t0)))))

(provide 'tempus)
;;; tempus.el ends here
