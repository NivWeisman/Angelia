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

(defun angelia-tests--collect-session-events (frames kind)
  "Return the list of frames in FRAMES whose params kind == KIND."
  (cl-remove-if-not
   (lambda (f) (equal (gethash "kind" (gethash "params" f)) kind))
   frames))

(defun angelia-tests--drive-file-read (path &optional chunk-size)
  "Call angelia-server--file-read on PATH, pump timers, return (SESSION FRAMES)."
  (let* ((params (make-hash-table :test #'equal))
         (_ (puthash "path" path params))
         (_ (when chunk-size (puthash "chunk-size" chunk-size params)))
         session
         (frames (angelia-tests-capture-responses
                   (let ((result (angelia-server--file-read nil params)))
                     (setq session (gethash "session" result))
                     (should (stringp session))
                     (with-timeout (5 (error "file/read timer did not fire"))
                       (while (gethash session angelia-server--sessions)
                         (accept-process-output nil 0.01)))))))
    (list session frames)))

(ert-deftest test-server-file-read ()
  "file/read streams the file as base64 chunks terminated by `end'."
  (clrhash angelia-server--sessions)
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "hello.txt" dir))
           (content "hello, angelia\n"))
      (angelia-tests-write-file path content)
      (pcase-let* ((`(,_session ,frames)
                    (angelia-tests--drive-file-read path)))
        (let* ((chunks (angelia-tests--collect-session-events frames "chunk"))
               (ends   (angelia-tests--collect-session-events frames "end"))
               (joined (apply #'concat
                              (mapcar (lambda (f)
                                        (base64-decode-string
                                         (gethash "data"
                                                  (gethash "params" f))))
                                      chunks))))
          (should (= 1 (length ends)))
          (should (>= (length chunks) 1))
          (should (equal joined content)))))))

(ert-deftest test-server-file-read-chunks-large ()
  "A 256 KB file read with 64 KB chunks yields exactly 4 chunk events + 1 end."
  (clrhash angelia-server--sessions)
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "big.bin" dir))
           (content (make-string (* 256 1024) ?A)))
      (angelia-tests-write-file path content)
      (pcase-let* ((`(,_session ,frames)
                    (angelia-tests--drive-file-read path (* 64 1024))))
        (let ((chunks (angelia-tests--collect-session-events frames "chunk"))
              (ends   (angelia-tests--collect-session-events frames "end")))
          (should (= 4 (length chunks)))
          (should (= 1 (length ends)))
          (let ((joined (apply #'concat
                               (mapcar (lambda (f)
                                         (base64-decode-string
                                          (gethash "data"
                                                   (gethash "params" f))))
                                       chunks))))
            (should (equal joined content))))))))

(defun angelia-tests--open-write-session (path size)
  "Drive file/write-open and return the session id."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "path" path p)
    (puthash "size" size p)
    (gethash "session" (angelia-server--file-write-open nil p))))

(defun angelia-tests--write-chunk (session bytes)
  "Drive file/write-chunk for SESSION with BYTES; return its result hash."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" session p)
    (puthash "data" (base64-encode-string bytes t) p)
    (angelia-server--file-write-chunk nil p)))

(defun angelia-tests--finish-write (session)
  "Drive file/write-finish for SESSION, swallowing the synthetic `end' event."
  (let ((p (make-hash-table :test #'equal)))
    (puthash "session" session p)
    (cl-letf (((symbol-function 'angelia-server--write-frame)
               (lambda (_) nil)))
      (angelia-server--file-write-finish nil p))))

(ert-deftest test-server-file-write ()
  "file/write-* writes base64-decoded bytes to disk in one chunk + atomic rename."
  (clrhash angelia-server--sessions)
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "out.txt" dir))
           (content "round-trip payload\n")
           (session (angelia-tests--open-write-session path (length content))))
      (should (= (gethash "accepted"
                          (angelia-tests--write-chunk session content))
                 (length content)))
      (should (= (gethash "written" (angelia-tests--finish-write session))
                 (length content)))
      (should (file-exists-p path))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (should (equal (buffer-string) content))))))

(ert-deftest test-server-file-write-multi-chunk ()
  "A 256 KB write streamed in four 64 KB chunks lands intact (SHA256 match)."
  (clrhash angelia-server--sessions)
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "multi.bin" dir))
           (size (* 256 1024))
           (content (let ((s (make-string size ?M)))
                      (aset s 0 ?A)
                      (aset s (1- size) ?Z)
                      s))
           (expected (secure-hash 'sha256 content))
           (session (angelia-tests--open-write-session path size)))
      (dotimes (i 4)
        (let* ((start (* i (* 64 1024)))
               (chunk (substring content start (+ start (* 64 1024)))))
          (angelia-tests--write-chunk session chunk)))
      (should (= (gethash "written" (angelia-tests--finish-write session))
                 size))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (should (equal (secure-hash 'sha256 (current-buffer)) expected))))))

(ert-deftest test-server-file-write-abandoned ()
  "Closing a write session without finish deletes the tmp + leaves target alone."
  (clrhash angelia-server--sessions)
  (angelia-tests-with-temp-dir dir
    (let* ((path (expand-file-name "abandoned.txt" dir))
           (session (angelia-tests--open-write-session path 10))
           (tmp (plist-get (gethash session angelia-server--sessions) :tmp)))
      (should (stringp tmp))
      (should (file-exists-p tmp))
      (angelia-tests--write-chunk session "1234567890")
      (let ((p (make-hash-table :test #'equal)))
        (puthash "session" session p)
        (cl-letf (((symbol-function 'angelia-server--write-frame)
                   (lambda (_) nil)))
          (angelia-server--builtin-session-close nil p)))
      (should-not (file-exists-p tmp))
      (should-not (file-exists-p path))
      (should-not (gethash session angelia-server--sessions)))))

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

;; ---------------------------------------------------------------------------
;; Phase 1 -- sessions.

(ert-deftest test-server-session-id-format ()
  "Session ids are `s-' + 16 lowercase hex characters."
  (dotimes (_ 50)
    (let ((id (angelia-server--make-session-id)))
      (should (string-match "\\`s-[0-9a-f]\\{16\\}\\'" id)))))

(ert-deftest test-server-send-notification ()
  "`angelia-server--send-notification' writes a frame with method+params and no id."
  (let* ((payload (let ((p (make-hash-table :test #'equal)))
                    (puthash "x" 1 p) p))
         (frames (angelia-tests-capture-responses
                   (angelia-server--send-notification
                    nil "test/notify" payload))))
    (should (= 1 (length frames)))
    (let ((f (car frames)))
      (should (equal (gethash "jsonrpc" f) "2.0"))
      (should (equal (gethash "method" f) "test/notify"))
      ;; The "id" key must be ABSENT (not just nil) -- a present nil id would
      ;; serialise as `{...,"id":null,...}' which is a response, not a
      ;; notification.
      (should-not (gethash "id" f))
      (should (equal (gethash "x" (gethash "params" f)) 1)))))

(ert-deftest test-server-end-session-emits-end ()
  "`angelia-server--end-session' emits one kind=end event and clears the row."
  (clrhash angelia-server--sessions)
  (let ((session "s-feedface00000001"))
    (angelia-server--register-session session (list :kind 'test))
    (should (gethash session angelia-server--sessions))
    (let ((frames (angelia-tests-capture-responses
                    (angelia-server--end-session nil session))))
      (should (= 1 (length frames)))
      (let* ((f (car frames))
             (p (gethash "params" f)))
        (should (equal (gethash "method" f) "session/event"))
        (should (equal (gethash "session" p) session))
        (should (equal (gethash "kind" p) "end"))))
    (should-not (gethash session angelia-server--sessions))))

(ert-deftest test-server-session-close-handler ()
  "The `session/close' built-in ends the named session and returns t."
  (clrhash angelia-server--sessions)
  (let ((session "s-feedface00000002"))
    (angelia-server--register-session session (list :kind 'test))
    (let* ((params (let ((p (make-hash-table :test #'equal)))
                     (puthash "session" session p) p))
           (frames (angelia-tests-capture-responses
                     (should (eq t (angelia-server--builtin-session-close
                                    nil params))))))
      (should (= 1 (length frames)))
      (should (equal (gethash "kind" (gethash "params" (car frames))) "end")))
    (should-not (gethash session angelia-server--sessions))))

(ert-deftest test-server-session-echo-direct ()
  "`session/echo' opens a session, emits N events with payload, then ends."
  (clrhash angelia-server--sessions)
  (let* ((params (let ((p (make-hash-table :test #'equal)))
                   (puthash "count" 4 p)
                   (puthash "payload" "marker" p)
                   p))
         session
         (frames (angelia-tests-capture-responses
                   (let ((result (angelia-server--builtin-session-echo
                                  nil params)))
                     (setq session (gethash "session" result))
                     (should (stringp session))
                     ;; Pump pending timers so the deferred run-at-time
                     ;; lambda fires and the events + end are captured.
                     (with-timeout (2 (error "session/echo timer did not fire"))
                       (while (gethash session angelia-server--sessions)
                         (accept-process-output nil 0.01)))))))
    (should (= 5 (length frames)))  ; 4 echo + 1 end
    (dotimes (i 4)
      (let* ((f (nth i frames))
             (p (gethash "params" f)))
        (should (equal (gethash "method" f) "session/event"))
        (should (equal (gethash "session" p) session))
        (should (equal (gethash "kind" p) "echo"))
        (should (equal (gethash "index" p) i))
        (should (equal (gethash "payload" p) "marker"))))
    (let* ((end-frame (nth 4 frames))
           (p (gethash "params" end-frame)))
      (should (equal (gethash "kind" p) "end"))
      (should (equal (gethash "session" p) session)))))

(ert-deftest test-server-session-echo-end-immediately ()
  "`session/echo' with end-immediately=t emits no events, just `end'."
  (clrhash angelia-server--sessions)
  (let* ((params (let ((p (make-hash-table :test #'equal)))
                   (puthash "count" 99 p)
                   (puthash "end-immediately" t p)
                   p))
         session
         (frames (angelia-tests-capture-responses
                   (let ((result (angelia-server--builtin-session-echo
                                  nil params)))
                     (setq session (gethash "session" result))
                     (with-timeout (2 (error "timer did not fire"))
                       (while (gethash session angelia-server--sessions)
                         (accept-process-output nil 0.01)))))))
    (should (= 1 (length frames)))
    (should (equal (gethash "kind" (gethash "params" (car frames))) "end"))))

;;; test-server-unit.el ends here
