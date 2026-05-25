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

## Interactive commands

| Command | Purpose |
|---|---|
| `M-x angelia-client-connect`         | Connect to a host explicitly (otherwise opening a remote file connects on demand). |
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
