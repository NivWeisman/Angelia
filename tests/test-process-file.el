;;; -*- lexical-binding: t; -*-
;;; test-process-file.el --- Layer 2 tests for process-file via Angelia

;; Exercises `process-file' and `start-file-process' against a
;; `default-directory' that resolves through the Angelia handler.  These
;; are the primitives magit and any other shell-out package rely on.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-client)
(require 'angelia-client-files)
(require 'angelia-client-proc)

(defconst angelia-tests--pf-host "localhost")

(defun angelia-tests--pf-remote-dir ()
  (concat "/@angelia:" angelia-tests--pf-host ":/tmp/"))

(defmacro angelia-tests--with-pf-buffer (buf &rest body)
  (declare (indent 1))
  `(let ((,buf (generate-new-buffer " *angelia-pf-test*")))
     (unwind-protect
         (with-current-buffer ,buf
           (setq default-directory (angelia-tests--pf-remote-dir))
           ,@body)
       (when (buffer-live-p ,buf) (kill-buffer ,buf)))))

;; ---------------------------------------------------------------------------

(ert-deftest test-process-file-echo ()
  "`process-file' \"echo hi\" returns exit 0 and inserts \"hi\\n\"."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (angelia-tests--with-pf-buffer buf
      (let ((exit (process-file "echo" nil t nil "hi")))
        (should (eq exit 0))
        (should (equal (buffer-string) "hi\n"))))))

(ert-deftest test-process-file-stdin-from-file ()
  "`process-file' \"cat\" with INFILE round-trips bytes through remote stdin."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (let ((local (make-temp-file "angelia-pf-stdin-")))
      (unwind-protect
          (progn
            (angelia-tests-write-file local "hello stdin\n")
            (angelia-tests--with-pf-buffer buf
              (let ((exit (process-file "cat" local t nil)))
                (should (eq exit 0))
                (should (equal (buffer-string) "hello stdin\n")))))
        (when (file-exists-p local) (delete-file local))))))

(ert-deftest test-process-file-nonzero-exit ()
  "`process-file' propagates non-zero exit status."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (angelia-tests--with-pf-buffer buf
      (let ((exit (process-file "sh" nil t nil "-c" "exit 7")))
        (should (eq exit 7))))))

(ert-deftest test-process-file-stderr-separation ()
  "When DESTINATION is `(REAL-BUF STDERR-FILE)' stderr goes to the file alone."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (let ((stderr-file (make-temp-file "angelia-pf-stderr-")))
      (unwind-protect
          (angelia-tests--with-pf-buffer buf
            (let ((exit (process-file
                         "sh" nil (list t stderr-file) nil
                         "-c" "printf out; printf err 1>&2")))
              (should (eq exit 0))
              (should (equal (buffer-string) "out"))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally stderr-file)
                (should (equal (buffer-string) "err")))))
        (when (file-exists-p stderr-file) (delete-file stderr-file))))))

(ert-deftest test-process-file-large-stdout ()
  "Emit ~1 MB of stdout via /usr/bin/yes-like loop; assert round-trip length."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (angelia-tests--with-pf-buffer buf
      (let* ((count (* 1024 1024))
             (exit (process-file
                    "sh" nil t nil
                    "-c" (format "head -c %d /dev/urandom | base64 -w0" count))))
        (should (eq exit 0))
        ;; base64 of N bytes is roughly 4*ceil(N/3); fine to just check length
        ;; is well over the chunk size to exercise multiple stdout chunks.
        (should (> (buffer-size) (* 1024 1024)))))))

(ert-deftest test-start-file-process-sentinel ()
  "`start-file-process' fires its sentinel with a `finished' event on success."
  (angelia-tests-ensure-no-connections)
  (with-angelia-connection angelia-tests--pf-host _conn
    (angelia-tests--with-pf-buffer buf
      (let* ((sentinel-events '())
             (proc (start-file-process "angelia-async-test" buf
                                       "sh" "-c" "echo async-hi")))
        (set-process-sentinel
         proc (lambda (_p event) (push event sentinel-events)))
        (with-timeout (10 (error "start-file-process timed out"))
          (while (process-live-p proc)
            (accept-process-output nil 0.05)))
        ;; Drain any pending notifications carrying the sentinel call.
        (accept-process-output nil 0.1)
        (should (member "finished\n" sentinel-events))
        (should (string-match-p "async-hi" (buffer-string)))))))

;;; test-process-file.el ends here
