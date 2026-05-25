;;; -*- lexical-binding: t; -*-
;;; test-server-unit.el --- Layer 0 unit tests for angelia-server.el

;; These tests exercise the server's internal machinery without any SSH or
;; network involvement.  The single exception is `test-server-stdout-clean'
;; which spawns the server in an `emacs --batch' subprocess to confirm that
;; nothing leaks onto stdout ahead of the first protocol frame.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'test-helpers)
(require 'angelia-server)

;; ---------------------------------------------------------------------------
;; Content-Length framing.

(ert-deftest test-server-json-parse ()
  "A complete Content-Length frame is peeled off the input buffer cleanly."
  (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"server/ping\"}")
         (frame (angelia-tests-build-frame body))
         (result (angelia-server--frame-bytes frame)))
    (should (consp result))
    (should (equal (decode-coding-string (car result) 'utf-8 t) body))
    (should (equal (cdr result) (unibyte-string)))))

(ert-deftest test-server-json-parse-incomplete ()
  "Partial frames return nil; once enough bytes arrive, a frame appears."
  (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"server/ping\"}")
         (frame (angelia-tests-build-frame body)))
    ;; Truncated header (no \r\n\r\n yet).
    (should (null (angelia-server--frame-bytes "Content-Length: 5")))
    ;; Header but body too short.
    (let ((short (substring frame 0 (- (length frame) 5))))
      (should (null (angelia-server--frame-bytes short))))
    ;; Full frame plus a partial second frame in the same buffer.
    (let* ((tail "Content-Length: 99\r\n\r\nINCOMPLETE")
           (combined (concat frame tail))
           (result (angelia-server--frame-bytes combined)))
      (should (consp result))
      (should (equal (decode-coding-string (car result) 'utf-8 t) body))
      ;; Remaining bytes preserve the partial second frame for the next read.
      (should (equal (cdr result) tail))
      (should (null (angelia-server--frame-bytes (cdr result)))))))

(ert-deftest test-server-json-serialize ()
  "Writing a frame produces a correctly-sized Content-Length header + body."
  (let* ((payload (make-hash-table :test #'equal))
         (_ (progn (puthash "jsonrpc" "2.0" payload)
                   (puthash "id" 42 payload)
                   (puthash "result" "pong" payload)))
         (raw (angelia-tests-capture-stdout-bytes
                (angelia-server--write-frame payload))))
    (should (= 1 (length raw)))
    (let* ((bytes (car raw))
           (declared (angelia-tests-extract-content-length bytes))
           (body (angelia-tests-extract-body bytes)))
      (should declared)
      (should (= declared (length body)))
      (should (string-prefix-p "Content-Length:" bytes))
      ;; Body parses back to the original payload.
      (let ((parsed (json-parse-string (decode-coding-string body 'utf-8 t)
                                       :object-type 'hash-table)))
        (should (equal (gethash "result" parsed) "pong"))
        (should (equal (gethash "id" parsed) 42))))))

;; ---------------------------------------------------------------------------
;; Dispatch / error envelopes.

(defun angelia-tests--make-frame-object (id method &optional params)
  "Build the parsed-envelope hash-table the dispatcher receives."
  (let ((h (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" h)
    (when id (puthash "id" id h))
    (puthash "method" method h)
    (when params (puthash "params" params h))
    h))

(ert-deftest test-server-method-dispatch ()
  "Registered handlers are invoked and their result is wrapped as a response."
  (let* ((calls '())
         (handler (lambda (conn params)
                    (push (cons conn params) calls)
                    "ok-result"))
         (frame (angelia-tests--make-frame-object 11 "test/echo" "hello")))
    (unwind-protect
        (progn
          (angelia-server-register-method "test/echo" handler)
          (let ((responses (angelia-tests-capture-responses
                             (angelia-server--dispatch 'fake-conn frame))))
            (should (= 1 (length responses)))
            (let ((resp (car responses)))
              (should (equal (gethash "id" resp) 11))
              (should (equal (gethash "result" resp) "ok-result"))
              (should (null (gethash "error" resp)))))
          (should (= 1 (length calls)))
          (should (equal (caar calls) 'fake-conn))
          (should (equal (cdar calls) "hello")))
      (angelia-server-unregister-method "test/echo"))))

(ert-deftest test-server-method-not-found ()
  "Unknown methods yield a JSON-RPC -32601 error response."
  (let* ((frame (angelia-tests--make-frame-object 22 "does/not/exist"))
         (responses (angelia-tests-capture-responses
                      (angelia-server--dispatch 'fake-conn frame))))
    (should (= 1 (length responses)))
    (let* ((resp (car responses))
           (err (gethash "error" resp)))
      (should (equal (gethash "id" resp) 22))
      (should err)
      (should (= (gethash "code" err) -32601))
      (should (string-match-p "does/not/exist" (gethash "message" err))))))

(ert-deftest test-server-handler-error ()
  "A handler that signals `error' produces a -32603 response."
  (let ((frame (angelia-tests--make-frame-object 33 "test/explode"))
        (handler (lambda (_conn _params) (error "kaboom"))))
    (unwind-protect
        (progn
          (angelia-server-register-method "test/explode" handler)
          (let ((responses (angelia-tests-capture-responses
                             (angelia-server--dispatch 'fake-conn frame))))
            (should (= 1 (length responses)))
            (let* ((resp (car responses))
                   (err (gethash "error" resp)))
              (should (equal (gethash "id" resp) 33))
              (should err)
              (should (= (gethash "code" err) -32603))
              (should (string-match-p "kaboom" (gethash "message" err))))))
      (angelia-server-unregister-method "test/explode"))))

(ert-deftest test-server-notification-no-response ()
  "Notifications (no id) never elicit a response, even for unknown methods."
  (let ((responses (angelia-tests-capture-responses
                     (angelia-server--dispatch 'fake-conn
                                               (angelia-tests--make-frame-object nil "no/id/method"))
                     (angelia-server--dispatch 'fake-conn
                                               (angelia-tests--make-frame-object :null "no/id/null")))))
    (should (null responses))))

(ert-deftest test-server-invalid-request-missing-method ()
  "A request envelope without a string `method' field yields -32600."
  (let ((frame (angelia-tests--make-frame-object 44 nil)))
    (remhash "method" frame)
    (let ((responses (angelia-tests-capture-responses
                       (angelia-server--dispatch 'fake-conn frame))))
      (should (= 1 (length responses)))
      (let* ((resp (car responses))
             (err (gethash "error" resp)))
        (should (= (gethash "code" err) -32600))))))

;; ---------------------------------------------------------------------------
;; Built-in handlers.

(ert-deftest test-server-builtin-ping ()
  "`server/ping' replies with pong=t and a timestamp."
  (let* ((conn (angelia-server--conn-create))
         (result (angelia-server--builtin-ping conn nil)))
    (should (eq (gethash "pong" result) t))
    (should (stringp (gethash "timestamp" result)))))

(ert-deftest test-server-builtin-version ()
  "`server/version' replies with sha1, emacs_version, and pid."
  (let* ((conn (angelia-server--conn-create))
         (result (angelia-server--builtin-version conn nil)))
    (should (stringp (gethash "sha1" result)))
    (should (= 40 (length (gethash "sha1" result))))
    (should (equal (gethash "emacs_version" result) emacs-version))
    (should (integerp (gethash "pid" result)))))

(ert-deftest test-server-builtin-info ()
  "`server/info' replies with the full diagnostic blob."
  (let* ((conn (angelia-server--conn-create))
         ;; `angelia-server--start-time' is nil unless `-main' has run.  Bind
         ;; it locally so uptime_ms is a sane non-negative integer.
         (angelia-server--start-time (current-time))
         (result (angelia-server--builtin-info conn nil)))
    (should (stringp (gethash "sha1" result)))
    (should (integerp (gethash "pid" result)))
    (should (stringp (gethash "hostname" result)))
    (should (integerp (gethash "uptime_ms" result)))
    (should (>= (gethash "uptime_ms" result) 0))))

;; ---------------------------------------------------------------------------
;; File-operation handlers — gated on Step 6 implementations.

(ert-deftest test-server-file-read ()
  "file/read returns base64-encoded file contents."
  (skip-unless (fboundp 'angelia-server--file-read))
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "hello.txt" dir))
           (content "hello, angelia\n"))
      (angelia-tests-write-file path content)
      (let* ((params (make-hash-table :test #'equal))
             (_ (puthash "path" path params))
             (result (funcall (symbol-function 'angelia-server--file-read)
                              (angelia-server--conn-create) params))
             (encoded (gethash "content" result))
             (decoded (base64-decode-string encoded)))
        (should (equal decoded content))))))

(ert-deftest test-server-file-write ()
  "file/write writes base64-decoded bytes to disk."
  (skip-unless (fboundp 'angelia-server--file-write))
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "out.txt" dir))
           (content "round-trip payload\n")
           (params (make-hash-table :test #'equal)))
      (puthash "path" path params)
      (puthash "content" (base64-encode-string content t) params)
      (funcall (symbol-function 'angelia-server--file-write)
               (angelia-server--conn-create) params)
      (should (file-exists-p path))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (should (equal (buffer-string) content))))))

(ert-deftest test-server-file-exists ()
  "file/exists is true for existing paths and false otherwise."
  (skip-unless (fboundp 'angelia-server--file-exists))
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "present.txt" dir))
           (missing (expand-file-name "absent.txt" dir))
           (mk (lambda (p)
                 (let ((h (make-hash-table :test #'equal)))
                   (puthash "path" p h) h))))
      (angelia-tests-write-file path "x")
      (should (eq t (funcall (symbol-function 'angelia-server--file-exists)
                             (angelia-server--conn-create) (funcall mk path))))
      (should (eq nil (funcall (symbol-function 'angelia-server--file-exists)
                               (angelia-server--conn-create) (funcall mk missing)))))))

(ert-deftest test-server-file-attributes ()
  "file/attributes returns size + type for a known file."
  (skip-unless (fboundp 'angelia-server--file-attributes))
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "attrs.txt" dir))
           (content "1234567890")
           (params (make-hash-table :test #'equal)))
      (angelia-tests-write-file path content)
      (puthash "path" path params)
      (let ((attrs (funcall (symbol-function 'angelia-server--file-attributes)
                            (angelia-server--conn-create) params)))
        (should (= (gethash "size" attrs) (length content)))
        (should (equal (gethash "type" attrs) "file"))))))

(ert-deftest test-server-directory-list ()
  "file/list-dir returns the directory entries."
  (skip-unless (fboundp 'angelia-server--file-list-dir))
  (angelia-tests-with-temp-dir dir
    (dolist (name '("a.txt" "b.txt" "c.txt"))
      (angelia-tests-write-file (expand-file-name name dir) name))
    (let ((params (make-hash-table :test #'equal)))
      (puthash "path" dir params)
      (let* ((result (funcall (symbol-function 'angelia-server--file-list-dir)
                              (angelia-server--conn-create) params))
             (entries (gethash "entries" result))
             (names (mapcar (lambda (e) (gethash "name" e)) (append entries nil))))
        (should (member "a.txt" names))
        (should (member "b.txt" names))
        (should (member "c.txt" names))))))

;; ---------------------------------------------------------------------------
;; Subprocess sanity: stdout has no preamble before the first protocol frame.

(ert-deftest test-server-stdout-clean ()
  "Spawning the server and sending one request yields stdout starting with
`Content-Length:' (no Emacs startup noise, no stray prints).  Bytes are
captured through a process filter so Emacs's process-status footer never
contaminates the result."
  (let* ((stderr-buf (generate-new-buffer " *angelia-test-stderr*"))
         (accumulator (cons (unibyte-string) nil))
         proc)
    (unwind-protect
        (progn
          (setq proc
                (make-process
                 :name "angelia-test-server"
                 :command (list "emacs" "-Q" "--batch"
                                "-L" angelia-tests--lisp-dir
                                "-l" angelia-tests--server-source
                                "-f" "angelia-server-main")
                 :filter (lambda (_p out)
                           (setcar accumulator (concat (car accumulator) out)))
                 :stderr stderr-buf
                 :coding 'binary
                 :connection-type 'pipe
                 :noquery t))
          (process-send-string proc (angelia-tests-build-request 1 "server/ping"))
          ;; Wait for at least one complete frame.
          (with-timeout (5 (error "Timed out waiting for server response"))
            (while (let ((s (car accumulator)))
                     (let ((declared (angelia-tests-extract-content-length s))
                           (sep (string-match "\r\n\r\n" s)))
                       (not (and declared sep
                                 (>= (- (length s) (+ sep 4)) declared)))))
              (accept-process-output proc 0.1)))
          (process-send-eof proc)
          (with-timeout (3 nil)
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          (let* ((out (car accumulator))
                 (sep (string-match "\r\n\r\n" out))
                 (declared (angelia-tests-extract-content-length out))
                 (body (substring out (+ sep 4) (+ sep 4 declared)))
                 (parsed (json-parse-string
                          (decode-coding-string body 'utf-8 t)
                          :object-type 'hash-table)))
            (should (> (length out) 0))
            (should (string-prefix-p "Content-Length:" out))
            (should (equal (gethash "id" parsed) 1))
            (should (eq (gethash "pong" (gethash "result" parsed)) t))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (kill-buffer stderr-buf))))

;;; test-server-unit.el ends here
