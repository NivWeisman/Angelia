;;; -*- lexical-binding: t; -*-
;;; test-notify.el --- Layer 2 tests for file-notify watches + honest modtime

;; Two related features that both rest on the server's `session/event' push
;; channel and a real remote `stat':
;;
;;   * `verify-visited-file-modtime' now compares the recorded remote mtime
;;     against the file's current remote mtime, so an external edit is detected
;;     instead of silently clobbered (it used to be hardwired to t).
;;   * `file-notify-add-watch' on an Angelia path opens a remote `file/watch'
;;     session; the server watches the real directory with inotify and streams
;;     `fsevent's that feed Emacs's `file-notify-callback' (auto-revert, etc.).
;;
;; Requires passwordless `ssh localhost'.  The watch test additionally needs a
;; working file-notification backend on the (localhost) server.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)

(defconst angelia-tests--notify-host "localhost")

(defun angelia-tests--notify-remote (path)
  (concat "/@angelia:" angelia-tests--notify-host ":" path))

;; ---------------------------------------------------------------------------
;; Honest modtime.

(ert-deftest test-notify-modtime-detects-external-change ()
  "`verify-visited-file-modtime' is t for a freshly read buffer and flips to nil
once the underlying file's mtime moves under it."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--notify-host _conn
    (let ((local (make-temp-file "angelia-modtime-" nil nil "v1\n")))
      (unwind-protect
          (let ((buf (find-file-noselect (angelia-tests--notify-remote local))))
            (unwind-protect
                (progn
                  ;; Freshly read: recorded mtime matches the file.
                  (should (eq t (verify-visited-file-modtime buf)))
                  ;; Move the file's mtime forward out-of-band.
                  (set-file-times local (time-add (current-time) 10))
                  ;; Now the buffer is stale -- verify must say so.
                  (should (null (verify-visited-file-modtime buf))))
              (when (buffer-live-p buf)
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf))))
        (when (file-exists-p local) (delete-file local))))))

(ert-deftest test-notify-modtime-save-resets ()
  "Saving the buffer refreshes the recorded mtime, so verify is t afterwards
\(our own write must not look like an external edit)."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--notify-host _conn
    (let ((local (make-temp-file "angelia-modtime-save-" nil nil "v1\n")))
      (unwind-protect
          (let ((buf (find-file-noselect (angelia-tests--notify-remote local))))
            (unwind-protect
                (with-current-buffer buf
                  ;; Angelia buffers visit read-only today (mirrors the
                  ;; `inhibit-read-only' workaround in test-save-file-remote).
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert "v2\n"))
                  (save-buffer)
                  (should (eq t (verify-visited-file-modtime buf))))
              (when (buffer-live-p buf)
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf))))
        (when (file-exists-p local) (delete-file local))))))

;; ---------------------------------------------------------------------------
;; file-notify watches.
;;
;; The watch *plumbing* (the file/watch + file/unwatch RPCs and the descriptor
;; lifecycle) is validated unconditionally.  Whether a change *event* actually
;; arrives depends on the server host's kernel delivering inotify events to
;; `emacs --batch' -- some sandboxed/virtualised environments compile inotify
;; in but never deliver, so the event-delivery assertions are gated on a quick
;; local probe (`angelia-tests--file-notify-works-p').

(defvar angelia-tests--fn-works 'unknown
  "Cached result of `angelia-tests--file-notify-works-p'.")

(defun angelia-tests--file-notify-works-p ()
  "Non-nil if the local environment actually delivers file-notify events.
inotify can be present yet silent in some sandboxes; in that case the watch
plumbing is still exercised but event delivery cannot be observed."
  (when (eq angelia-tests--fn-works 'unknown)
    (setq angelia-tests--fn-works
          (and file-notify--library
               (let ((dir (make-temp-file "fn-probe-" t)) (hit nil) (desc nil))
                 (unwind-protect
                     (progn
                       (setq desc (file-notify-add-watch
                                   dir '(change) (lambda (_e) (setq hit t))))
                       (write-region "x" nil (expand-file-name "p" dir) nil 'silent)
                       (with-timeout (2 nil)
                         (while (not hit) (accept-process-output nil 0.05)))
                       hit)
                   (when desc (ignore-errors (file-notify-rm-watch desc)))
                   (ignore-errors (delete-directory dir t)))))))
  angelia-tests--fn-works)

(defun angelia-tests--notify-wait (pred &optional timeout)
  "Pump process output until PRED returns non-nil or TIMEOUT (default 10s)."
  (with-timeout ((or timeout 10) nil)
    (while (not (funcall pred))
      (accept-process-output nil 0.1))
    t))

(ert-deftest test-notify-watch-plumbing ()
  "Adding a watch on a remote path opens a live `file/watch' session (descriptor
valid); rm-watch closes it (descriptor invalid).  No fs event required."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--notify-host _conn
    (let ((dir (file-name-as-directory (make-temp-file "angelia-watch-pl-" t))))
      (unwind-protect
          (let ((desc (file-notify-add-watch
                       (angelia-tests--notify-remote dir) '(change) #'ignore)))
            (should (consp desc))
            (should (eq (car desc) 'angelia-fnotify))
            (should (file-notify-valid-p desc))
            (file-notify-rm-watch desc)
            (should-not (file-notify-valid-p desc)))
        (when (file-directory-p dir) (delete-directory dir t))))))

(ert-deftest test-notify-watch-fires-on-change ()
  "A file watch delivers a change event (named by basename) when the file is
edited out-of-band."
  (skip-unless (angelia-tests--file-notify-works-p))
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--notify-host _conn
    (let* ((dir (file-name-as-directory (make-temp-file "angelia-watch-" t)))
           (file (expand-file-name "watched.txt" dir)))
      (unwind-protect
          (let* ((events '())
                 (desc (file-notify-add-watch
                        (angelia-tests--notify-remote file)
                        '(change)
                        (lambda (event) (push event events)))))
            (unwind-protect
                (progn
                  (angelia-tests-write-file file "changed\n")
                  (should (angelia-tests--notify-wait
                           (lambda () (cl-some
                                       (lambda (e)
                                         (equal (file-name-nondirectory (nth 2 e))
                                                "watched.txt"))
                                       events)))))
              (file-notify-rm-watch desc)))
        (when (file-directory-p dir) (delete-directory dir t))))))

(ert-deftest test-notify-watch-directory-fires ()
  "A directory watch reports a newly created entry by basename."
  (skip-unless (angelia-tests--file-notify-works-p))
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--notify-host _conn
    (let ((dir (file-name-as-directory (make-temp-file "angelia-watchdir-" t))))
      (unwind-protect
          (let* ((events '())
                 (desc (file-notify-add-watch
                        (angelia-tests--notify-remote dir)
                        '(change)
                        (lambda (event) (push event events)))))
            (unwind-protect
                (progn
                  (angelia-tests-write-file (expand-file-name "fresh.txt" dir) "x")
                  (should (angelia-tests--notify-wait
                           (lambda ()
                             (cl-some (lambda (e)
                                        (equal (file-name-nondirectory (nth 2 e))
                                               "fresh.txt"))
                                      events)))))
              (file-notify-rm-watch desc)))
        (when (file-directory-p dir) (delete-directory dir t))))))

;;; test-notify.el ends here
