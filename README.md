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

**A 68k Macintosh** (68020 and up, 8 MB), bare metal, booting modus from two floppies:

| file | what it is |
|---|---|
| `kernel/mvm/mac68k-files.lisp` | the `FLOPPY` package: mount, dir, read, write, load files on a FAT12 floppy (over fs-fat) |
| `kernel/mvm/mac68k-disk-io.lisp` | the image's side of the Mac disk service (read or write a drive, select one) |
| `kernel/scripts/mac68k-bootblocks.s` | the boot blocks: the memory check, then the launch record's read |
| `kernel/scripts/mac68k-unlzma.s`, `mac68k-lzma.inc` | the floppy loader and its raw LZMA1 decoder (`test/mac68k-unlzma/run.sh` checks it against xz and cram) |
| `kernel/scripts/mac68k-lzdisk.s` | the driver that serves the core disk's cold code from 64 KB LZMA blocks on disk 2 |
| `kernel/scripts/mac68k-disk2.s` | "insert disk 2", for a Mac with one drive |
| `kernel/scripts/mac68k-scsi.s`, `mac68k-chain.s` | the hard-disk boot, and the relocating loader of modus as a Mac OS application |
| `tools/mac68k/` | the workbench: `period-lisps/` boots MCL 2.0, Pearl Lisp and Modus on MAME's Macs and types forms at them (System 6 / 7.5.3 volumes built with fs-apple's HFS); `prof/` reads MAME's profiles; `mame-run.sh` drives the REPL by its screen.  Its README says how the emulators were set up |

`kernel/` is at the paths modus's builds name them by (`kernel/mvm/mac68k-files.lisp` is
modus's `"mvm/mac68k-files.lisp"`); modus's Mac disk scripts assemble the `.s` files from
here.  The image itself -- `boot/boot-mac68k.lisp`, the 68k translator's cold code,
`mvm/mac68k-cold.lisp` -- stays in modus: it changes with the compiler.

kiln assembles images from these and never carries what goes into one: the container clones
vessel at its `repos.lock` pin and loads `src/one.lisp`; `kiln ios` / `kiln android` load
`src/app.lisp` and the platform file into the world they snapshot; `kiln zero` builds a card from these at the commit `repos.lock` pins (or from a workspace
checkout with `--workspace`): it copies them, with the libraries they load, into one
`kiln-zero` system the board installs.

The history before November 2026 is kiln's, carried over with the files.
