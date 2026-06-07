;;; -*- lexical-binding: t; -*-
;;; compare-tramp.el --- Benchmark + correctness suite: Angelia vs TRAMP

;; A side-by-side comparison harness for the two remote-file backends:
;;
;;   * Angelia   -- /@angelia:HOST:/path  (this project)
;;   * TRAMP     -- /sshx:HOST:/path      (built into Emacs)
;;
;; Both target the SAME localhost paths over ssh, so every operation does real
;; equivalent work and the results are directly comparable.  For each operation
;; the suite does two things:
;;
;;   1. CORRECTNESS -- runs the op through both backends and asserts they agree
;;      (same bytes read, same on-disk effect, same directory listing).  This
;;      is what makes it a *test* suite, not just a benchmark: if Angelia ever
;;      diverges from TRAMP's observable behaviour, a test fails.
;;
;;   2. TIMING -- runs the op N times per backend and records the median wall
;;      time, measured through Tempus.  TRAMP method calling is wrapped with
;;      Tempus via tests/tramp-tempus.el, mirroring how Angelia already wraps
;;      every RPC task; both backends therefore feed one Tempus stream that the
;;      harness harvests.
;;
;; The report covers FILE OPERATIONS and DIRED (the current scope).
;;
;; This file is deliberately NOT named `test-*.el', so `run-all.el's glob does
;; not pull it into the default `make test' / per-layer runs.  It is the one
;; place in the repo that uses TRAMP (CLAUDE.md hard rule 4 bans TRAMP from the
;; product, lisp/ -- this is a tests/ benchmark, the explicit exception).  Run
;; it on its own:
;;
;;     make compare
;;     ANGELIA_COMPARE_ITERS=9 make compare      ; more samples, steadier medians
;;
;; Requires passwordless `ssh localhost' and a remote `emacs' on PATH, same as
;; the Layer-2 tests; it skips cleanly when those are unavailable.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'tramp)
(require 'tempus)
(require 'tramp-tempus)
(require 'angelia-client)
(require 'angelia-client-files)

;; ---------------------------------------------------------------------------
;; Configuration.

(defvar angelia-compare-host "localhost"
  "Host both backends connect to.  localhost keeps the comparison hermetic:
the `remote' path is the same path on the local disk, so we can prepare and
verify state directly while still exercising the full ssh round-trip.")

(defvar angelia-compare-tramp-method "sshx"
  "TRAMP method used for the comparison.
`sshx' forces a pty plus `/bin/sh -i' (`ssh -t -t -o RemoteCommand=...'), which
is what makes TRAMP work against hosts whose login shell is zsh/csh -- exactly
the environments Angelia's login-wrap targets.  The plain `ssh' method hangs on
such hosts (it never matches the remote shell prompt), so it would not give a
comparable, working baseline.")

(defvar angelia-compare-iterations
  (let ((env (getenv "ANGELIA_COMPARE_ITERS")))
    (if (and env (> (string-to-number env) 0)) (string-to-number env) 5))
  "Timed repetitions per operation per backend; the report shows the median.
Override with the ANGELIA_COMPARE_ITERS environment variable.")

(defvar angelia-compare--rows nil
  "Cached result of the last full matrix run.
Each element is a plist: (:name :section :tramp-ms :angelia-ms :match).  The
ERT tests assert over this; `angelia-compare-run' populates it.")

(defvar angelia-compare--cold nil
  "Plist of connection cold-start costs measured once at run start:
(:tramp-ms :angelia-ms).  Informational -- printed, not asserted.")

(defvar angelia-compare--skip-reason nil
  "Non-nil (a string) when the backends are unavailable and the run was skipped.")

(defvar angelia-compare--tmpdir nil
  "Scratch directory holding every file the matrix touches, on localhost.")

(defvar angelia-compare--counter 0
  "Monotonic counter for unique scratch path names.")

;; ---------------------------------------------------------------------------
;; Tempus-backed timing primitives.  Per CLAUDE.md we never hand-roll
;; `(float-time (time-subtract ...))' for new timing -- every measurement here
;; flows through `tempus-measure'; we just install a capturing log function to
;; harvest the milliseconds it produces.

(defun angelia-compare--timed (label thunk)
  "Return the elapsed ms of (funcall THUNK), measured via Tempus under LABEL.
Captures only the outer LABEL line, so the nested Tempus lines that TRAMP's
per-method advice and Angelia's per-RPC `tempus-measure' emit during the body
are ignored -- LABEL is unique to this call (see `angelia-compare--bench-op')."
  (let* ((ms nil)
         (tempus-debug t)
         (tempus-log-function
          (lambda (_fmt l m) (when (equal l label) (setq ms m)))))
    (tempus-measure label (funcall thunk))
    (or ms (error "tempus produced no timing for %S" label))))

(defun angelia-compare--median (xs)
  "Median of the numeric list XS (0 for the empty list)."
  (let* ((s (sort (copy-sequence xs) #'<))
         (n (length s)))
    (cond ((zerop n) 0)
          ((cl-oddp n) (float (nth (/ n 2) s)))
          (t (/ (+ (nth (1- (/ n 2)) s) (nth (/ n 2) s)) 2.0)))))

;; ---------------------------------------------------------------------------
;; Scratch-state helpers (all on localhost == the remote host).

(defun angelia-compare--url (backend path)
  "Wrap local PATH as a remote URL for BACKEND (`tramp' or `angelia')."
  (pcase backend
    ('tramp   (format "/%s:%s:%s" angelia-compare-tramp-method
                      angelia-compare-host path))
    ('angelia (concat "/@angelia:" angelia-compare-host ":" path))
    (_ (error "Unknown backend %S" backend))))

(defun angelia-compare--fresh (suffix)
  "Return a fresh, non-existent absolute path under the scratch dir."
  (expand-file-name
   (format "c%d-%s" (cl-incf angelia-compare--counter) suffix)
   angelia-compare--tmpdir))

(defun angelia-compare--write-local (path content)
  "Write CONTENT (unibyte) to local PATH with binary coding."
  (let ((coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert content)
      (write-region (point-min) (point-max) path nil 'silent))))

(defun angelia-compare--slurp (path)
  "Return PATH's bytes as a unibyte string, or :missing if it is absent."
  (if (file-exists-p path)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (buffer-string))
    :missing))

(defun angelia-compare--make-blob (n)
  "Deterministic N-byte ASCII blob with sentinels at both ends."
  (let ((s (make-string n ?x)))
    (when (> n 0) (aset s 0 ?A) (aset s (1- n) ?Z))
    s))

(defun angelia-compare--make-dir (names)
  "Create a fresh scratch directory containing NAMES (empty files); return it."
  (let ((dir (file-name-as-directory (angelia-compare--fresh "dir"))))
    (make-directory dir t)
    (dolist (n names) (angelia-compare--write-local (expand-file-name n dir) "x"))
    dir))

;; ---------------------------------------------------------------------------
;; Operation matrix.
;;
;; Each op is a plist:
;;   :name      report label
;;   :section   :file or :dired
;;   :mutates   non-nil if the op changes disk state (=> fresh ctx per run)
;;   :make      (lambda () -> CTX): build disk state, return a plist with :path
;;   :run       (lambda (MKURL CTX) -> RESULT): perform the op.  MKURL maps a
;;              local path to this backend's URL, so the op is backend-agnostic.
;;   :norm      (lambda (RESULT CTX) -> COMPARABLE): normalise for the equality
;;              check; defaults to the raw RESULT.  Mutating ops read the disk
;;              effect here since their RESULT is nil.

(defun angelia-compare--ops ()
  "Return the full operation matrix (file operations followed by dired)."
  (list
   ;; ---- FILE OPERATIONS --------------------------------------------------
   (list :name "file-exists-p (present)" :section :file
         :make (lambda () (let ((p (angelia-compare--fresh "exists")))
                            (angelia-compare--write-local p "x") (list :path p)))
         :run  (lambda (mk c) (file-exists-p (funcall mk (plist-get c :path)))))
   (list :name "file-exists-p (absent)" :section :file
         :make (lambda () (list :path (angelia-compare--fresh "absent")))
         :run  (lambda (mk c) (file-exists-p (funcall mk (plist-get c :path)))))
   (list :name "file-readable-p" :section :file
         :make (lambda () (let ((p (angelia-compare--fresh "rd")))
                            (angelia-compare--write-local p "x") (list :path p)))
         :run  (lambda (mk c) (and (file-readable-p (funcall mk (plist-get c :path))) t)))
   (list :name "file-writable-p" :section :file
         :make (lambda () (let ((p (angelia-compare--fresh "wr")))
                            (angelia-compare--write-local p "x") (list :path p)))
         :run  (lambda (mk c) (and (file-writable-p (funcall mk (plist-get c :path))) t)))
   (list :name "file-attributes (type+size)" :section :file
         :make (lambda () (let ((p (angelia-compare--fresh "attr")))
                            (angelia-compare--write-local p "0123456789") (list :path p)))
         :run  (lambda (mk c)
                 (let ((a (file-attributes (funcall mk (plist-get c :path)))))
                   (list (file-attribute-type a) (file-attribute-size a)))))
   (list :name "insert-file-contents (1 KB)" :section :file
         :make (lambda () (let ((p (angelia-compare--fresh "rsm")))
                            (angelia-compare--write-local p (angelia-compare--make-blob 1024))
                            (list :path p)))
         :run  (lambda (mk c)
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents (funcall mk (plist-get c :path)))
                   (secure-hash 'sha256 (current-buffer)))))
   (list :name "insert-file-contents (512 KB)" :section :file :iters 3
         :make (lambda () (let ((p (angelia-compare--fresh "rlg")))
                            (angelia-compare--write-local p (angelia-compare--make-blob (* 512 1024)))
                            (list :path p)))
         :run  (lambda (mk c)
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents (funcall mk (plist-get c :path)))
                   (secure-hash 'sha256 (current-buffer)))))
   (list :name "write-region (1 KB)" :section :file :mutates t
         :make (lambda () (list :path (angelia-compare--fresh "wsm")
                                :content (angelia-compare--make-blob 1024)))
         :run  (lambda (mk c)
                 (write-region (plist-get c :content) nil
                               (funcall mk (plist-get c :path)) nil 'silent))
         :norm (lambda (_r c) (angelia-compare--slurp (plist-get c :path))))
   (list :name "write-region (512 KB)" :section :file :mutates t :iters 3
         :make (lambda () (list :path (angelia-compare--fresh "wlg")
                                :content (angelia-compare--make-blob (* 512 1024))))
         :run  (lambda (mk c)
                 (write-region (plist-get c :content) nil
                               (funcall mk (plist-get c :path)) nil 'silent))
         :norm (lambda (_r c) (secure-hash 'sha256 (angelia-compare--slurp (plist-get c :path)))))
   (list :name "delete-file" :section :file :mutates t
         :make (lambda () (let ((p (angelia-compare--fresh "del")))
                            (angelia-compare--write-local p "x") (list :path p)))
         :run  (lambda (mk c) (delete-file (funcall mk (plist-get c :path))))
         :norm (lambda (_r c) (file-exists-p (plist-get c :path))))
   (list :name "make-directory" :section :file :mutates t
         :make (lambda () (list :path (angelia-compare--fresh "mkd")))
         :run  (lambda (mk c) (make-directory (funcall mk (plist-get c :path))))
         :norm (lambda (_r c) (and (file-directory-p (plist-get c :path)) t)))
   (list :name "copy-file" :section :file :mutates t
         :make (lambda () (let ((s (angelia-compare--fresh "cps")))
                            (angelia-compare--write-local s "copy me\n")
                            (list :path s :dst (angelia-compare--fresh "cpd"))))
         :run  (lambda (mk c) (copy-file (funcall mk (plist-get c :path))
                                         (funcall mk (plist-get c :dst))))
         :norm (lambda (_r c) (angelia-compare--slurp (plist-get c :dst))))
   (list :name "rename-file" :section :file :mutates t
         :make (lambda () (let ((s (angelia-compare--fresh "rns")))
                            (angelia-compare--write-local s "rename me\n")
                            (list :path s :dst (angelia-compare--fresh "rnd"))))
         :run  (lambda (mk c) (rename-file (funcall mk (plist-get c :path))
                                           (funcall mk (plist-get c :dst))))
         :norm (lambda (_r c) (cons (file-exists-p (plist-get c :path))
                                    (angelia-compare--slurp (plist-get c :dst)))))
   ;; ---- DIRED ------------------------------------------------------------
   (list :name "directory-files" :section :dired
         :make (lambda () (list :path (angelia-compare--make-dir '("a" "b" "c"))))
         :run  (lambda (mk c)
                 (sort (directory-files (funcall mk (plist-get c :path)) nil nil 'nosort)
                       #'string<)))
   (list :name "directory-files-and-attributes" :section :dired
         :make (lambda () (list :path (angelia-compare--make-dir '("a" "b" "c"))))
         :run  (lambda (mk c)
                 (sort (mapcar #'car
                               (directory-files-and-attributes
                                (funcall mk (plist-get c :path)) nil nil 'nosort))
                       #'string<)))
   (list :name "file-name-all-completions" :section :dired
         :make (lambda () (list :path (angelia-compare--make-dir
                                       '("apple" "apricot" "banana"))))
         :run  (lambda (mk c)
                 (sort (file-name-all-completions "ap" (funcall mk (plist-get c :path)))
                       #'string<)))
   (list :name "insert-directory (-la)" :section :dired
         :make (lambda () (list :path (angelia-compare--make-dir '("marker.txt"))))
         :run  (lambda (mk c)
                 (with-temp-buffer
                   (insert-directory (funcall mk (plist-get c :path)) "-la" nil t)
                   (let ((s (buffer-string)))
                     (and (string-match-p "marker\\.txt" s)
                          (string-match-p "^total " s) t)))))
   (list :name "dired-noselect" :section :dired
         :make (lambda () (list :path (angelia-compare--make-dir '("entry.txt"))))
         :run  (lambda (mk c)
                 (let ((buf (dired-noselect (funcall mk (plist-get c :path)))))
                   (unwind-protect
                       (and (string-match-p "entry\\.txt"
                                            (with-current-buffer buf (buffer-string)))
                            t)
                     (when (buffer-live-p buf) (kill-buffer buf))))))))

(defun angelia-compare--norm (op result ctx)
  "Apply OP's :norm to RESULT/CTX (identity when OP has no :norm)."
  (let ((fn (plist-get op :norm)))
    (if fn (funcall fn result ctx) result)))

;; ---------------------------------------------------------------------------
;; Running one op.

(defun angelia-compare--correctness (op)
  "Run OP through both backends and return the normalised (TRAMP . ANGELIA) pair.
Non-mutating ops share one disk state (literally the same bytes); mutating ops
get a fresh state per backend so the first run does not perturb the second."
  (let ((mk-t (lambda (p) (angelia-compare--url 'tramp p)))
        (mk-a (lambda (p) (angelia-compare--url 'angelia p))))
    (if (plist-get op :mutates)
        (let* ((ct (funcall (plist-get op :make)))
               (rt (angelia-compare--norm op (funcall (plist-get op :run) mk-t ct) ct))
               (ca (funcall (plist-get op :make)))
               (ra (angelia-compare--norm op (funcall (plist-get op :run) mk-a ca) ca)))
          (cons rt ra))
      (let* ((c (funcall (plist-get op :make)))
             (rt (angelia-compare--norm op (funcall (plist-get op :run) mk-t c) c))
             (ra (angelia-compare--norm op (funcall (plist-get op :run) mk-a c) c)))
        (cons rt ra)))))

(defun angelia-compare--bench-op (op backend)
  "Return the median ms of OP on BACKEND over `angelia-compare-iterations' runs.
A unique Tempus label (`bench:BACKEND:NAME') isolates this measurement from the
backends' own nested Tempus lines.  Mutating ops rebuild fresh disk state each
iteration (outside the timed region); stable ops reuse one state."
  (let* ((mk (lambda (p) (angelia-compare--url backend p)))
         (run (plist-get op :run))
         (n (or (plist-get op :iters) angelia-compare-iterations))
         (label (format "bench:%s:%s" backend (plist-get op :name)))
         (shared (unless (plist-get op :mutates) (funcall (plist-get op :make))))
         samples)
    (dotimes (_ n)
      (let ((ctx (or shared (funcall (plist-get op :make)))))
        (push (angelia-compare--timed label (lambda () (funcall run mk ctx)))
              samples)))
    (angelia-compare--median samples)))

(defun angelia-compare--run-op (op)
  "Run correctness + timing for OP; return its result row plist."
  (let ((pair (angelia-compare--correctness op)))
    (list :name (plist-get op :name)
          :section (plist-get op :section)
          :match (equal (car pair) (cdr pair))
          :tramp-ms (angelia-compare--bench-op op 'tramp)
          :angelia-ms (angelia-compare--bench-op op 'angelia))))

;; ---------------------------------------------------------------------------
;; Availability, setup, warmup, teardown.

(defun angelia-compare--available-p ()
  "Best-effort check that both backends can reach the host; return nil if not.
Sets `angelia-compare--skip-reason' on failure (ssh / remote emacs missing)."
  (condition-case err
      (let* ((probe (angelia-compare--fresh "probe")))
        (angelia-compare--write-local probe "ok\n")
        ;; Angelia connect + a read.
        (angelia-client-connect angelia-compare-host)
        (unless (file-exists-p (angelia-compare--url 'angelia probe))
          (error "angelia could not see probe file"))
        ;; TRAMP read.
        (unless (file-exists-p (angelia-compare--url 'tramp probe))
          (error "tramp could not see probe file"))
        t)
    (error
     (setq angelia-compare--skip-reason (error-message-string err))
     nil)))

(defun angelia-compare--cold-start ()
  "Measure connection cold-start cost for each backend; return a plist.
TRAMP: tear down its connections, then time the first op (ssh + shell
negotiation included).  Angelia: disconnect, then time connect + first op."
  (let ((known (angelia-compare--fresh "cold")))
    (angelia-compare--write-local known "x")
    (ignore-errors (tramp-cleanup-all-connections))
    (ignore-errors (angelia-client-disconnect angelia-compare-host))
    (list
     :tramp-ms
     (ignore-errors
       (angelia-compare--timed
        "cold:tramp"
        (lambda () (file-exists-p (angelia-compare--url 'tramp known)))))
     :angelia-ms
     (ignore-errors
       (angelia-compare--timed
        "cold:angelia"
        (lambda ()
          (angelia-client-connect angelia-compare-host)
          (file-exists-p (angelia-compare--url 'angelia known))))))))

(defun angelia-compare--warmup ()
  "Establish both connections so per-op timings exclude the cold-start cost."
  (let ((p (angelia-compare--fresh "warm")))
    (angelia-compare--write-local p "x")
    (angelia-client-connect angelia-compare-host)
    (file-exists-p (angelia-compare--url 'angelia p))
    (file-exists-p (angelia-compare--url 'tramp p))))

(defun angelia-compare--setup ()
  "Create the scratch dir and quiet TRAMP down for the run."
  (setq angelia-compare--tmpdir
        (file-name-as-directory (make-temp-file "angelia-compare-" t))
        angelia-compare--counter 0
        angelia-compare--skip-reason nil)
  (setq tramp-verbose 0)
  (tramp-tempus-install))

(defun angelia-compare--teardown ()
  "Drop connections, remove instrumentation, delete the scratch dir."
  (tramp-tempus-uninstall)
  (ignore-errors (angelia-client-disconnect angelia-compare-host))
  (ignore-errors (tramp-cleanup-all-connections))
  (when (and angelia-compare--tmpdir (file-directory-p angelia-compare--tmpdir))
    (ignore-errors (delete-directory angelia-compare--tmpdir t)))
  (setq angelia-compare--tmpdir nil))

(defun angelia-compare--run-matrix ()
  "Run the full matrix under TRAMP's file-attribute cache disabled.
Binding `remote-file-name-inhibit-cache' to t makes TRAMP actually round-trip
every metadata/listing op instead of answering from its cache, so the medians
reflect real work on both sides (Angelia has no such cache).  Stores rows in
`angelia-compare--rows'."
  (let ((remote-file-name-inhibit-cache t))
    (setq angelia-compare--rows
          (mapcar #'angelia-compare--run-op (angelia-compare--ops)))))

;; ---------------------------------------------------------------------------
;; TRAMP internal breakdown -- the literal payoff of wrapping TRAMP method
;; calling with Tempus: one user-level op fans out into many TRAMP sub-ops,
;; each now timed.  This is why TRAMP is slower per user operation.

(defun angelia-compare--tramp-breakdown (path)
  "Read PATH via TRAMP, capturing every `tramp <op>' Tempus line in order."
  (let* ((cap nil)
         (tempus-debug t)
         (tempus-log-function (lambda (_fmt l m) (push (cons l m) cap))))
    (with-temp-buffer
      (insert-file-contents (angelia-compare--url 'tramp path)))
    (nreverse cap)))

;; ---------------------------------------------------------------------------
;; Report.

(defun angelia-compare--fmt-ms (ms)
  "Format MS for the table (or `--' when nil)."
  (if (numberp ms) (format "%9.1f" ms) (format "%9s" "--")))

(defun angelia-compare--print-section (rows section title)
  "Print the table block for ROWS belonging to SECTION under TITLE.
Returns (TRAMP-SUM . ANGELIA-SUM) of the medians, for the totals line."
  (let ((tsum 0) (asum 0) (any nil))
    (princ (format "\n %s\n" title))
    (princ (format " %-34s %9s %11s %9s  %s\n"
                   "operation" "TRAMP ms" "Angelia ms" "speedup" "match"))
    (princ (format " %s\n" (make-string 78 ?-)))
    (dolist (row rows)
      (when (eq (plist-get row :section) section)
        (setq any t)
        (let* ((tms (plist-get row :tramp-ms))
               (ams (plist-get row :angelia-ms))
               (spd (if (and (numberp tms) (numberp ams) (> ams 0))
                        (format "%7.1fx" (/ tms ams)) "     --"))
               (match (if (plist-get row :match) "ok" "DIFF!")))
          (when (numberp tms) (setq tsum (+ tsum tms)))
          (when (numberp ams) (setq asum (+ asum ams)))
          (princ (format " %-34s %s %s %s  %s\n"
                         (plist-get row :name)
                         (angelia-compare--fmt-ms tms)
                         (angelia-compare--fmt-ms ams)
                         spd match)))))
    (when any
      (princ (format " %s\n" (make-string 78 ?-)))
      (princ (format " %-34s %9.1f %11.1f %7.1fx\n"
                     "median sum" tsum asum
                     (if (> asum 0) (/ tsum asum) 0))))
    (cons tsum asum)))

(defun angelia-compare-print-report ()
  "Print the full Angelia-vs-TRAMP report from `angelia-compare--rows'."
  (princ (format "\n%s\n" (make-string 80 ?=)))
  (princ " Angelia vs TRAMP  --  file operations & dired\n")
  (princ (format " host=%s  tramp-method=%s  iterations=%d  emacs=%s\n"
                 angelia-compare-host angelia-compare-tramp-method
                 angelia-compare-iterations emacs-version))
  (princ (format "%s\n" (make-string 80 ?=)))
  (if angelia-compare--skip-reason
      (princ (format "\n SKIPPED: %s\n" angelia-compare--skip-reason))
    (progn
      (angelia-compare--print-section angelia-compare--rows :file  "FILE OPERATIONS")
      (angelia-compare--print-section angelia-compare--rows :dired "DIRED")
      ;; Cold start.
      (princ (format "\n %s\n" (make-string 78 ?-)))
      (let ((tc (plist-get angelia-compare--cold :tramp-ms))
            (ac (plist-get angelia-compare--cold :angelia-ms)))
        (princ (format " connection cold start:   TRAMP %s ms    Angelia %s ms\n"
                       (if (numberp tc) (format "%.0f" tc) "n/a")
                       (if (numberp ac) (format "%.0f" ac) "n/a"))))
      ;; TRAMP internal fan-out for one read.
      (let* ((p (angelia-compare--fresh "brk")))
        (angelia-compare--write-local p (angelia-compare--make-blob 1024))
        (let ((lines (ignore-errors (angelia-compare--tramp-breakdown p))))
          (when lines
            (princ "\n TRAMP method fan-out for ONE insert-file-contents")
            (princ " (each line = one TRAMP method call, Tempus-timed):\n")
            (dolist (e lines)
              (princ (format "     %-32s %8.1f ms\n" (car e) (cdr e))))
            (princ (format "   -> %d TRAMP method calls for a single user op\n"
                           (length lines))))))
      (princ "\n notes:\n")
      (princ "   - speedup = TRAMP median / Angelia median (>1.0x => Angelia faster).\n")
      (princ "   - metadata/listing ops timed with remote-file-name-inhibit-cache=t,\n")
      (princ "     so TRAMP actually round-trips instead of serving its attr cache.\n")
      (princ "   - cold start excluded from per-op medians (both connections warmed first).\n")))
  (princ (format "%s\n" (make-string 80 ?=)))
  (princ "\n"))

;; ---------------------------------------------------------------------------
;; ERT suite.  These assert over the matrix that `angelia-compare-run' already
;; populated; run them via `make compare'.

(ert-deftest test-compare-backends-available ()
  "Both backends reached the host (otherwise the whole comparison is skipped)."
  (skip-unless (null angelia-compare--skip-reason))
  (should angelia-compare--rows))

(ert-deftest test-compare-file-ops-agree ()
  "Every FILE-OPERATION produces the same observable result on both backends."
  (skip-unless angelia-compare--rows)
  (dolist (row angelia-compare--rows)
    (when (eq (plist-get row :section) :file)
      (should (plist-get row :match)))))

(ert-deftest test-compare-dired-ops-agree ()
  "Every DIRED operation produces the same observable result on both backends."
  (skip-unless angelia-compare--rows)
  (dolist (row angelia-compare--rows)
    (when (eq (plist-get row :section) :dired)
      (should (plist-get row :match)))))

(ert-deftest test-compare-timings-captured ()
  "Tempus produced a positive median for both backends on every op."
  (skip-unless angelia-compare--rows)
  (dolist (row angelia-compare--rows)
    (should (numberp (plist-get row :tramp-ms)))
    (should (> (plist-get row :tramp-ms) 0))
    (should (numberp (plist-get row :angelia-ms)))
    (should (> (plist-get row :angelia-ms) 0))))

;; ---------------------------------------------------------------------------
;; Entry point.

;;;###autoload
(defun angelia-compare-run ()
  "Run the comparison: warm up, benchmark, print the report, run the ERT suite.
Exits Emacs with a non-zero status if any ERT test fails.  This is the function
`make compare' invokes."
  (angelia-compare--setup)
  (let ((failures 0))
    (unwind-protect
        (if (not (angelia-compare--available-p))
            (progn
              (angelia-compare-print-report)   ; prints the SKIPPED banner
              (princ (format "compare: skipped (%s)\n" angelia-compare--skip-reason)))
          (setq angelia-compare--cold (angelia-compare--cold-start))
          (angelia-compare--warmup)
          (angelia-compare--run-matrix)
          (angelia-compare-print-report)
          (let ((stats (ert-run-tests-batch "\\`test-compare-")))
            (setq failures (ert-stats-completed-unexpected stats))))
      (angelia-compare--teardown))
    (kill-emacs (if (and (integerp failures) (> failures 0)) 1 0))))

(provide 'compare-tramp)
;;; compare-tramp.el ends here
