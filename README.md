# Angelia

A split-client Emacs Remote-SSH package, in the spirit of VS Code's Remote-SSH.

- **Local Emacs** owns the UI, buffers, and editing.
- **Remote Emacs** (`emacs --batch`) owns file I/O, directory listing, and command execution on the remote host.
- They talk **JSON-RPC 2.0** with LSP-style `Content-Length` framing over a single **SSH stdio pipe** — no TCP ports, no daemon, no TRAMP.

## Prerequisites

- Emacs **27 or later**, both locally and on every remote host (29.3+ recommended).
- **OpenSSH** client locally, OpenSSH server on every remote host.
- Passwordless `ssh <host>` (key-based auth, jump hosts, custom ports, `~/.ssh/config` aliases all work — they're handed straight to `ssh`).

## Install

```elisp
(add-to-list 'load-path "/path/to/Angelia/lisp")
(require 'angelia-client)
```

That's it. No remote-side installation — the package deploys its server to `~/.cache/angelia/server.el` on first connect.

## Usage

Open a remote file:

```
C-x C-f /@angelia:HOST:/absolute/path
```

`HOST` is whatever you'd pass to `ssh` (`localhost`, `myhost`, `user@host`, an `~/.ssh/config` alias, etc.). `dired` works on `/@angelia:HOST:/some/dir/` as well.

## External-change detection & auto-revert

Angelia records each visited file's real remote mtime, so `save-buffer` warns
before clobbering a file that changed underneath you instead of silently
overwriting it. `file-notify-add-watch` is implemented over a remote
`file/watch` session (the server watches the real path and streams change
events), so `auto-revert-mode` and log-tailing work on `/@angelia:` buffers
wherever the remote host delivers file-notification events.

## Resilience

Connections carry ssh keepalive (`ServerAliveInterval`/`CountMax`) so a dead
link is noticed instead of hanging on a half-open socket. When a connection
drops unexpectedly, Angelia reconnects transparently — the next request
re-establishes the link, and a background retry with exponential backoff runs in
parallel — then re-registers any live `file-notify` watches against the fresh
connection. An explicit `M-x angelia-client-disconnect` is never auto-reconnected.
Tunables: `angelia-client-auto-reconnect`, `angelia-client-keepalive-interval`,
`angelia-client-reconnect-max-attempts`.

## Remote search

`M-x angelia-grep` runs `rg` (or `grep`) **on the remote host** and streams the
hits into a `grep-mode` buffer — RET jumps to a match through the file handler.
One `file/search` session does the whole tree, instead of fanning out a
round-trip per file. Defaults to `default-directory` when it is already an
Angelia path; capped by `angelia-client-files-search-max`.

## Interactive commands

| Command | Purpose |
|---|---|
| `M-x angelia-client-connect`         | Connect to a host explicitly (otherwise opening a remote file connects on demand). |
| `M-x angelia-grep`                   | Stream `rg`/`grep` results from the remote host into a `grep-mode` buffer. |
| `M-x angelia-client-disconnect`      | Tear down a connection. |
| `M-x angelia-client-send-ping`       | Round-trip ping; reports milliseconds. |
| `M-x angelia-client-server-info`     | Show remote Emacs version, server hash, PID, uptime. |
| `M-x angelia-client-show-debug-log`  | Switch to `*angelia-client-debug*`. |
| `M-x angelia-client-clear-debug-log` | Clear the debug buffer. |

## Tests

```sh
make test            # all three layers
make test-unit       # Layer 0 — server unit tests, no SSH
make test-transport  # Layer 1 — SSH localhost lifecycle, ping, deploy
make test-files      # Layer 2 — find-file, dired, large file
```

Layers 1 and 2 require working passwordless `ssh localhost`.

## Status

Under active development. See [the plan](.claude/plans/) and `CLAUDE.md` for the design and current scope.
