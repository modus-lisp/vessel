# vessel

A kiln fires vessels. kiln assembles modus-lisp images; **vessel is what runs on the
device**: the per-device glue that makes a bare-metal modus board into a working machine.
Anything that differs between hosted modus and bare-metal modus belongs here, not in modus
and not in kiln.

Today that is the Raspberry Pi Zero 2 W:

| file | what it is |
|---|---|
| `src/zero.lisp` | the platform layer: HDMI screen, USB keyboard and mouse, the screen and audio threads, under the desk app |
| `src/zero-boot.lisp` | what a card does at power-on, before the desk: network, SSH, the owner's message |
| `src/zero-bt.lisp` | bordeaux-threads over modus's SB-THREAD on the board |
| `src/zero-card.lisp` | the SD card the board booted from, mounted (FAT32 over modus's SD driver) |
| `src/zero-ssh.lisp` | an SSH client over the board's own TCP |
| `src/zero-shell.lisp` | the board's SSH shell, edited with quill |
| `src/zero-term.lisp` | the desk's Terminal as a real terminal |
| `src/zero-rfb.lisp`, `src/zero-rfbs.lisp` | the desk as an RFB server, over the gateway link or the board's sockets |
| `src/zero-gateway.lisp` | the nostr gateway on the board |

`kiln zero` builds a card from these at the commit `repos.lock` pins (or from a workspace
checkout with `--workspace`): it copies them, with the libraries they load, into one
`kiln-zero` system the board installs.

The history before November 2026 is kiln's, carried over with the files.
