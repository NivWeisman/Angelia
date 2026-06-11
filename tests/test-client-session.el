;;; -*- lexical-binding: t; -*-
;;; test-client-session.el --- Layer 0 tests for session event queue + replay

;; Pure, SSH-free tests for the registration-race machinery in
;; angelia-client.el.  The race: jsonrpc.el's process filter dispatches every
;; complete message in one pass, so a method response and its first
;; `session/event' notifications can ALL be processed before the synchronous
;; requester regains control to register the session id the response carries.
;; Dropping those events loses chunks / exit events (a fast `proc/exec' then
;; hangs until its timeout); the client must queue and replay them instead.
;; These tests drive `angelia-client--handle-session-event' directly against
;; a fake connection row -- no server involved.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)

(defmacro angelia-client-tests--with-fake-conn (host conn-var &rest body)
  "Bind CONN-VAR to a fake conn registered for HOST; always deregister after."
  (declare (indent 2))
  `(let ((,conn-var (angelia-client--conn-create :host ,host)))
     (unwind-protect
         (progn
           (puthash ,host ,conn-var angelia-client--connections)
           ,@body)
       (remhash ,host angelia-client--connections))))

(ert-deftest test-client-session-events-before-registration-replay ()
  "Events arriving before registration are queued, then replayed in order."
  (angelia-client-tests--with-fake-conn "replay-host" conn
    (let ((seen '())
          (ended nil))
      ;; Three events land before any callback exists for s-1.
      (angelia-client--handle-session-event
       "replay-host" '(:session "s-1" :kind "chunk" :data "AAA"))
      (angelia-client--handle-session-event
       "replay-host" '(:session "s-1" :kind "chunk" :data "BBB"))
      (angelia-client--handle-session-event
       "replay-host" '(:session "s-1" :kind "end"))
      ;; Queued, not dispatched, not dropped.
      (should (null seen))
      (should (gethash "s-1" (angelia-client--conn-pending-events conn)))
      ;; Registration replays everything in arrival order.
      (angelia-client-register-session
       conn "s-1"
       (lambda (kind p) (push (cons kind (plist-get p :data)) seen))
       (lambda (_p) (setq ended t)))
      (should (equal '(("chunk" . "AAA") ("chunk" . "BBB")) (nreverse seen)))
      (should ended)
      ;; The replayed `end' tore the registration down; queue is drained.
      (should (null (gethash "s-1" (angelia-client--conn-sessions conn))))
      (should (null (gethash "s-1" (angelia-client--conn-pending-events conn)))))))

(ert-deftest test-client-session-registered-dispatches-directly ()
  "With a registered callback, events dispatch immediately (nothing queues)."
  (angelia-client-tests--with-fake-conn "direct-host" conn
    (let ((seen '()))
      (angelia-client-register-session
       conn "s-2" (lambda (kind _p) (push kind seen)) nil)
      (angelia-client--handle-session-event
       "direct-host" '(:session "s-2" :kind "output" :data "x"))
      (should (equal '("output") seen))
      (should (null (gethash "s-2" (angelia-client--conn-pending-events conn)))))))

(ert-deftest test-client-session-closed-tombstone-drops-late-events ()
  "Late events for a deliberately-closed session are dropped, not queued.
Without the tombstone, the still-streaming server side of an abandoned
session would fill the pending queue for nothing."
  (angelia-client-tests--with-fake-conn "tomb-host" conn
    (angelia-client-register-session conn "s-3" #'ignore nil)
    (angelia-client-deregister-session conn "s-3")
    (angelia-client--handle-session-event
     "tomb-host" '(:session "s-3" :kind "output" :data "zzz"))
    (should (null (gethash "s-3" (angelia-client--conn-pending-events conn))))
    (should (null (gethash "s-3" (angelia-client--conn-sessions conn))))))

(ert-deftest test-client-session-pending-queue-capped ()
  "The per-session pending queue stops growing at the configured cap."
  (angelia-client-tests--with-fake-conn "cap-host" conn
    (let ((angelia-client--pending-events-max 5))
      (dotimes (i 9)
        (angelia-client--handle-session-event
         "cap-host" (list :session "s-4" :kind "chunk" :data (format "%d" i))))
      (let ((cell (gethash "s-4" (angelia-client--conn-pending-events conn))))
        (should cell)
        (should (= 5 (length (cdr cell))))))))

;;; test-client-session.el ends here
