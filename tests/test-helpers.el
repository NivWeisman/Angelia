;;; -*- lexical-binding: t; -*-
;;; test-helpers.el --- Shared utilities for Angelia ERT tests

;; Loaded first by tests/run-all.el so every test file can `(require
;; 'test-helpers)' or just reference the helpers below.  No test logic here;
;; only fixtures, macros, and small builders.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Project paths (computed from this file's location).

(defvar angelia-tests--this-file
  (or load-file-name buffer-file-name)
  "Absolute path of test-helpers.el, captured at load time.")

(defvar angelia-tests--project-root
  (file-name-as-directory
   (directory-file-name
    (file-name-directory
     (directory-file-name (file-name-directory angelia-tests--this-file)))))
  "Repo root (one level above tests/).")

(defvar angelia-tests--lisp-dir
  (expand-file-name "lisp" angelia-tests--project-root)
  "Absolute path to the lisp/ source directory.")

(defvar angelia-tests--server-source
  (expand-file-name "angelia-server.el" angelia-tests--lisp-dir)
  "Absolute path to angelia-server.el (used by subprocess tests).")

;; ---------------------------------------------------------------------------
;; Filesystem helpers.

(defmacro angelia-tests-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then delete the directory."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "angelia-test-" t))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,var) (delete-directory ,var t)))))

(defun angelia-tests-write-file (path content)
  "Write CONTENT (a unibyte or multibyte string) to PATH using binary coding."
  (let ((coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert content)
      (write-region (point-min) (point-max) path nil 'silent))))

;; ---------------------------------------------------------------------------
;; Frame builders (Content-Length-framed JSON-RPC envelopes).

(defun angelia-tests-build-frame (json-string)
  "Wrap JSON-STRING in a Content-Length frame, returning unibyte bytes."
  (let* ((body (encode-coding-string json-string 'utf-8 t))
         (header (encode-coding-string
                  (format "Content-Length: %d\r\n\r\n" (length body))
                  'utf-8 t)))
    (concat header body)))

(defun angelia-tests-build-request (id method &optional params)
  "Build a JSON-RPC request frame (unibyte bytes) for ID/METHOD/PARAMS."
  (let ((req (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" req)
    (puthash "id" id req)
    (puthash "method" method req)
    (when params (puthash "params" params req))
    (angelia-tests-build-frame (json-serialize req))))

(defun angelia-tests-build-notification (method &optional params)
  "Build a JSON-RPC notification frame (unibyte bytes) for METHOD/PARAMS."
  (let ((req (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" req)
    (puthash "method" method req)
    (when params (puthash "params" params req))
    (angelia-tests-build-frame (json-serialize req))))

(defun angelia-tests-extract-content-length (frame-bytes)
  "Return the Content-Length value declared in FRAME-BYTES, or nil."
  (when (string-match "Content-Length:[ \t]*\\([0-9]+\\)" frame-bytes)
    (string-to-number (match-string 1 frame-bytes))))

(defun angelia-tests-extract-body (frame-bytes)
  "Return the body bytes after the header separator in FRAME-BYTES."
  (when-let ((sep (string-match "\r\n\r\n" frame-bytes)))
    (substring frame-bytes (+ sep 4))))

;; ---------------------------------------------------------------------------
;; Mocking helpers (Layer 0 — no subprocesses involved).

(defmacro angelia-tests-capture-responses (&rest body)
  "Run BODY with `angelia-server--write-frame' replaced by a recorder.
Evaluates to the list of response payloads written during BODY, in order."
  (declare (indent 0))
  (let ((acc (gensym "captured-")))
    `(let ((,acc '()))
       (cl-letf (((symbol-function 'angelia-server--write-frame)
                  (lambda (payload) (push payload ,acc))))
         ,@body)
       (nreverse ,acc))))

(defmacro angelia-tests-capture-stdout-bytes (&rest body)
  "Run BODY with `send-string-to-terminal' replaced by a recorder.
Evaluates to the list of raw byte strings that would have hit stdout."
  (declare (indent 0))
  (let ((acc (gensym "captured-bytes-")))
    `(let ((,acc '()))
       (cl-letf (((symbol-function 'send-string-to-terminal)
                  (lambda (s) (push s ,acc))))
         ,@body)
       (nreverse ,acc))))

;; ---------------------------------------------------------------------------
;; Connection bracket for Layer 1 / Layer 2 tests.

(defmacro with-angelia-connection (host conn-var &rest body)
  "Connect to HOST, bind CONN-VAR to the connection, run BODY, then disconnect.
Disconnects in an `unwind-protect' cleanup so a failing assertion does not
leak a live ssh subprocess into subsequent tests."
  (declare (indent 2))
  `(let ((,conn-var nil))
     (unwind-protect
         (progn
           (setq ,conn-var (angelia-client-connect ,host))
           ,@body)
       (when (gethash ,host angelia-client--connections)
         (angelia-client-disconnect ,host)))))

(defun angelia-tests-ensure-no-connections ()
  "Forcibly tear down every entry in `angelia-client--connections'.
Useful in test setup hooks so a half-leaked connection from a previous test
does not contaminate the next one."
  (dolist (host (hash-table-keys angelia-client--connections))
    (ignore-errors (angelia-client-disconnect host))))

(provide 'test-helpers)
;;; test-helpers.el ends here
