;;; -*- lexical-binding: t; -*-
;;; tramp-tempus.el --- Wrap TRAMP method calling with Tempus timing

;; Test-only instrumentation.  Angelia self-instruments every RPC task with
;; `tempus-measure' (see `angelia-client-call' and the server dispatch loop);
;; TRAMP has no such hook.  This module gives TRAMP the *same* treatment by
;; advising its file-name-handler so every operation TRAMP dispatches is timed
;; through the identical Tempus path -- one timing line per TRAMP method call,
;; keyed `tramp <operation>'.
;;
;; That makes the two backends measurable on equal footing: the comparison
;; harness (tests/compare-tramp.el) installs a capturing `tempus-log-function'
;; and harvests both `tramp <op>' and Angelia's own `client call <method>'
;; lines from the one Tempus stream.
;;
;; This is the ONLY place in the repo that touches TRAMP, and it lives under
;; tests/ by design -- the product (lisp/) never depends on TRAMP (CLAUDE.md
;; hard rule 4).  The advice is opt-in (`tramp-tempus-install') and fully
;; reversible (`tramp-tempus-uninstall'); loading this file installs nothing.

(require 'tempus)
(require 'tramp)

(defun tramp-tempus--handler-advice (orig operation &rest args)
  "Run ORIG TRAMP handler for OPERATION/ARGS, timing the dispatch via Tempus.
Installed as `:around' advice on `tramp-file-name-handler'.  The body is the
real handler call, so the recorded ms is TRAMP's full cost for that one
operation (including any nested handler ops it fans out to -- those nest as
their own `tramp <op>' lines, exactly mirroring how Angelia's server logs each
sub-call)."
  (tempus-measure (format "tramp %s" operation)
    (apply orig operation args)))

(defun tramp-tempus-install ()
  "Advise `tramp-file-name-handler' so every TRAMP method call is Tempus-timed.
Idempotent.  Timing still only emits when `tempus-debug' is non-nil and a
`tempus-log-function' is installed -- this just adds the measurement points."
  (interactive)
  (advice-add 'tramp-file-name-handler :around #'tramp-tempus--handler-advice))

(defun tramp-tempus-uninstall ()
  "Remove the Tempus timing advice from `tramp-file-name-handler'."
  (interactive)
  (advice-remove 'tramp-file-name-handler #'tramp-tempus--handler-advice))

(defmacro tramp-tempus-with-instrumentation (&rest body)
  "Run BODY with TRAMP method calling Tempus-instrumented, then uninstall."
  (declare (indent 0))
  `(progn
     (tramp-tempus-install)
     (unwind-protect (progn ,@body)
       (tramp-tempus-uninstall))))

(provide 'tramp-tempus)
;;; tramp-tempus.el ends here
