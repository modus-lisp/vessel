# vessel

A kiln fires vessels. kiln assembles modus-lisp images; **vessel is what runs**: the app,
the desktop and the per-device glue -- everything that goes INTO an image.  kiln only
assembles; anything that differs between hosted and bare-metal modus belongs here, not in
modus and not in kiln.

**The app** -- what a person uses, on whatever device it runs:

| file | what it is |
|---|---|
| `src/app.lisp` | the device app: clock, notes, terminal (with `ssh`), media, on a glass desk, over a small platform interface (`%kiln-sys` calls 1001-1012) |
| `src/ios.lisp`, `src/android.lisp` | that interface on a phone, over hosted modus (modus's `host/ios`, `host/android` answer the calls) |

**The desktop** -- the glass desktop and its gateway as one image:

| file | what it is |
|---|---|
| `src/one.lisp` | the desktop and its gateway in one Lisp image; loads its siblings below from beside itself |
| `src/session.lisp` | a session is an identity: its key, its name, where it lives |
| `src/sshd.lisp`, `src/config.lisp` | the control plane: conch's SSH server, and the menuconfig it serves |
| `src/ops.lisp` | the image described to whoever is inside it |
| `src/debugger.lisp` | a thread that dies does not take the desktop with it |
| `src/klog.lisp` | the event log, as private nostr DMs |
| `src/modus-rfb.lisp` | glass's RFB server on modus, on a given port |

**The Raspberry Pi Zero 2 W**, bare metal:

| file | what it is |
|---|---|
| `src/zero.lisp` | the platform layer: HDMI screen, USB keyboard and mouse, the screen and audio threads, under the app |
| `src/zero-boot.lisp` | what a card does at power-on, before the desk: network, SSH, the owner's message |
| `src/zero-bt.lisp` | bordeaux-threads over modus's SB-THREAD on the board |
| `src/zero-card.lisp` | the SD card the board booted from, mounted (FAT32 over modus's SD driver) |
| `src/zero-ssh.lisp` | an SSH client over the board's own TCP |
| `src/zero-shell.lisp` | the board's SSH shell, edited with quill |
| `src/zero-term.lisp` | the desk's Terminal as a real terminal |
| `src/zero-rfb.lisp`, `src/zero-rfbs.lisp` | the desk as an RFB server, over the gateway link or the board's sockets |
| `src/zero-gateway.lisp` | the nostr gateway on the board |

kiln assembles images from these and never carries what goes into one: the container clones
vessel at its `repos.lock` pin and loads `src/one.lisp`; `kiln ios` / `kiln android` load
`src/app.lisp` and the platform file into the world they snapshot; `kiln zero` builds a card from these at the commit `repos.lock` pins (or from a workspace
checkout with `--workspace`): it copies them, with the libraries they load, into one
`kiln-zero` system the board installs.

The history before November 2026 is kiln's, carried over with the files.
