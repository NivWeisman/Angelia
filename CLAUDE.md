# CLAUDE.md — Angelia developer invariants

Re-read this every session before touching code. These rules are non-negotiable; the user has called each one out explicitly. If you find yourself wanting to break one, **stop and ask** instead.

## What this project is

A split-client Emacs Remote-SSH package, like VS Code's Remote-SSH. Local Emacs is the UI; remote `emacs --batch` is the I/O backend. They speak **JSON-RPC 2.0** with **`Content-Length`-framed messages** (the LSP wire format) over a single **SSH stdio pipe**. The transport is wrapped with the built-in `jsonrpc-process-connection`.

## Repo layout

- `lisp/angelia-server.el` — runs on the remote host under `emacs --batch`. Deployed to `~/.cache/angelia/server.el` on first connect.
- `lisp/angelia-client.el` — local entry point. SSH process + jsonrpc connection lifecycle.
- `lisp/angelia-client-deploy.el` — version check, deploy, sha1 handshake.
- `lisp/angelia-client-files.el` — `file-name-handler-alist` entry for `/@angelia:HOST:/path`.
- `lisp/tempus.el` — standalone, zero-dependency timing utility (`tempus-measure`). Canonical source; the server **inlines** an equivalent copy in `angelia-server.el` (single-file deploy, see Tempus rule below).
- `tests/test-server-unit.el` (Layer 0) — no SSH.
- `tests/test-transport.el` (Layer 1) — SSH localhost.
- `tests/test-file-ops.el` (Layer 2) — find-file / dired / large file.
- `tests/test-helpers.el`, `tests/run-all.el` — shared utilities and runner.

## Hard rules

1. **Stdout on `angelia-server.el` is sacred.** Only `Content-Length`-framed JSON-RPC goes there. Every debug log, warning, `message`, error trace, *anything else* goes to **stderr** via `(princ … 'external-debugging-output)`. **Flush stdout after every response** — batch-mode Emacs stdio is buffered and the local client will hang if you don't.
2. **JSON via native C functions only.** `json-parse-string` and `json-serialize`. Never `json-read` / `json-encode` (the older `json.el`).
3. **Async event loop on the server.** `(while (not quit-flag) (accept-process-output nil 0.1))` — never synchronous read-respond. Every method handler is `(lambda (conn params) …)` — pass `conn` even when no handler uses it. That's what makes future LSP-proxy / PTY / streaming features cost-free; we don't build them now, but the shape costs nothing.
4. **Do NOT use:** `emacs --daemon` (detaches from stdio), TRAMP (the point is to replace it), TCP/UDP ports, `url.el` or HTTP, external JSON tools (jq, python). SSH stdio + native JSON only.
5. **SSH host strings are opaque.** Pass them straight to `ssh`. Never parse them yourself — `~/.ssh/config` aliases, jump hosts, `user@host:port`, key files, etc. all work transparently if you don't touch the string.
   - **Route every remote command through `angelia-client--login-wrap`** (lisp/angelia-client-deploy.el), never a hardcoded `bash --login -c`. `ssh host CMD` runs CMD under the remote's *default* login shell, **non-login**; that shell may not put emacs on `PATH` on a bare `-c`. `login-wrap` detects the shell family (`echo $SHELL`, cached) and loads the login env *its* way before `exec bash`: **csh/tcsh** → source `/etc/csh.login` + `~/.login` (path_helper → PATH); **zsh** (modern macOS default) → source `/etc/zprofile` + `~/.zprofile` (this is the *only* place `brew shellenv` puts `/opt/homebrew/bin` on PATH, and `bash --login` never reads it); **sh/bash** → `bash --login`. Every login-file `source` redirects its own stdout/stderr to `/dev/null` so a noisy profile can't corrupt the JSON-RPC stream. Wrapped commands must contain **no single quotes** (csh single-quoting is literal).
6. **Base64 everything.** All file content over the wire is base64-encoded in both directions. No exceptions — null bytes, invalid UTF-8, and JSON edge cases all go away.
7. **Every `.el` file starts with `;;; -*- lexical-binding: t; -*-`.** No exceptions, source or test.
8. **Symbol prefixes:** `angelia-server--` (private) / `angelia-server-` (public) on the remote; `angelia-client--` / `angelia-client-` on the local. Don't mix them.
9. **`shell-quote-argument` every remote path** you pass to a shell command.

## Debug-first

Every subsystem must be observable. Bare minimum logging:

- **Server (stderr only):** startup banner with version hash + Emacs version + PID + debug state; every raw stdin frame (truncated 500); dispatch (method, handler); handler elapsed ms (via Tempus — debug-gated, logged on success *and* error); every raw stdout frame (truncated); every flush; full backtraces on error.
- **Client (`*angelia-client-debug*` buffer):** connection attempt + deploy phase + SSH process start (full command line); sentinel events; every RPC request (id, method, truncated params); every RPC response (id, result/error truncated); file handler dispatch (op, path); per-request round-trip timing (via Tempus — debug-gated).

When something fails, the debug log should contain enough to diagnose it without re-running.

**Tempus timing.** Wrap every RPC task in `tempus-measure` (label + body) — client calls (`angelia-client-call`, the handshake, `angelia-client-async`) and the server handler `funcall` in `angelia-server--dispatch`. For start/end that straddle callbacks use `tempus-log-since`. Timing logs **only in debug mode** (`tempus-debug`), driven locally by the `angelia-client-debug` defcustom and remotely by the `ANGELIA_DEBUG` env var (the client prepends `ANGELIA_DEBUG=1` to the remote launch when its own debug is on, so the two stay in lockstep). It is logged on both success and error. Use Tempus to find bottlenecks; **never hand-roll `(float-time (time-subtract …))` for new timing** — call `tempus-measure`. Debug mode gates *only* Tempus timing; the rest of the logging above stays always-on. The canonical source is `lisp/tempus.el`; the server inlines an equivalent section to keep the single-file deploy + SHA1 handshake intact, so **edit both copies together**.

**Latency simulation (testing only).** Set `angelia-client-simulated-delay-ms` (>0) to make the client launch the server with `ANGELIA_DELAY_MS=N`; the server then sleeps that many ms once per request in `angelia-server--dispatch` (`angelia-server--simulate-delay`), so a localhost connection mimics a real, latent link. The delay sits *outside* the handler's `tempus-measure`, so server handler timing stays honest while the client round-trip reflects it. Never enable it in normal use.

## Build order and gates

Build strictly in the order in the plan (`/home/vboxuser/.claude/plans/project-emacs-remote-ssh-modular-karp.md`). **Stop and check in with the user after Steps 0, 2, 5, and 8.** Do not jump ahead. Do not combine steps.

## Deferred — do NOT build

LSP proxying, remote PTY/shell, session/stream IDs, process persistence (dtach/screen integration), chunked file transfer. The async loop + `conn`-to-handlers shape already accommodates them later.
