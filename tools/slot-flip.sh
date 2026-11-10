#!/usr/bin/env bash
# slot-flip.sh — make the Zero's tryboot slot its default, from inside that slot, over SSH.
#
#   tools/slot-flip.sh
#
# The card's config.txt names the default slot and tryboot.txt the other (tools/slot-install.sh
# writes into that one).  Once the tryboot slot has booted and works, flipping is one write:
# config.txt takes tryboot.txt's settings, and tryboot.txt takes config.txt's, so the old default
# stays one tryboot away.  tryboot.txt is written first -- the firmware reads it only on a
# tryboot -- then config.txt; the old pair is kept on the store as /config.before and
# /tryboot.before.
#
# REFUSES unless the board is running the tryboot slot now (the CORE= it booted with names that
# slot's core): a slot becomes the default only from inside it, after it has proved it boots.
#
# Env: ZERO_HOST, ZERO_GATEWAY as tools/board-load.sh.
set -euo pipefail
ZH=${ZERO_HOST:-10.0.0.199} GW=${ZERO_GATEWAY:-modus@modus-pi}
SSHO=(-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=30 -o BatchMode=yes)
zeval() { ssh "${SSHO[@]}" -J "$GW" "modus@$ZH" "$1"; }
slot_of() {
  zeval "(let* ((s (map (quote string) (function code-char) (fs-fat:fat32-read-file (kiln-zero-card::%volume) \"/$1\"))) (i (search \"kernel=kernel8-\" s))) (if i (string-downcase (string (char s (+ i 15)))) \"none\"))" \
    | tr -dc 'a-z'
}
new=$(slot_of tryboot.txt) old=$(slot_of config.txt)
case "$new$old" in ab|ba) ;; *) echo "slot-flip: config.txt boots '$old' and tryboot.txt '$new'; expected one of each" >&2; exit 1 ;; esac
running=$(zeval '(let ((c (funcall (intern "%BOOTARG" "KILN-ZERO-BOOT") "CORE"))) (if c (string-downcase c) "none"))' | tr -d '"= \r')
[ "$running" = "2:/boot/desk-$new.core" ] || {
  echo "slot-flip: the board is running '$running', not slot $new's core: boot slot $new with (modus-int::reboot :tryboot t) and flip from there" >&2; exit 1; }
form=$(sed "s/@NEW@/$new/g; s/@OLD@/$old/g" <<'FORM' | tr '\n' ' '
(let* ((v (kiln-zero-card::%volume))
       (st (kiln-zero-card::%store))
       (cfg (fs-fat:fat32-read-file v "/config.txt"))
       (try (fs-fat:fat32-read-file v "/tryboot.txt"))
       (relabel (lambda (bytes comment)
                  (let* ((s (map 'string #'code-char bytes))
                         (i (search "# slot " s))
                         (s2 (if i (concatenate 'string (subseq s 0 i) comment
                                                (subseq s (or (position #\Newline s :start i) (length s))))
                                 s)))
                    (map '(vector (unsigned-byte 8)) #'char-code s2)))))
  (fs-fat:fat32-write-file st "/config.before" cfg)
  (fs-fat:fat32-write-file st "/tryboot.before" try)
  (fs-fat:fat32-sync st)
  (fs-fat:fat32-write-file v "/tryboot.txt"
    (funcall relabel cfg "# slot @OLD@: the firmware reads this file instead of config.txt on a tryboot"))
  (fs-fat:fat32-sync v)
  (fs-fat:fat32-write-file v "/config.txt"
    (funcall relabel try "# slot @NEW@ is the default; tryboot.txt is slot @OLD@"))
  (fs-fat:fat32-sync v)
  (let ((c (map 'string #'code-char (fs-fat:fat32-read-file v "/config.txt")))
        (tb (map 'string #'code-char (fs-fat:fat32-read-file v "/tryboot.txt"))))
    (list :default (and (search "kernel=kernel8-@NEW@.img" c) t)
          :tryboot (and (search "kernel=kernel8-@OLD@.img" tb) t))))
FORM
)
echo "slot-flip: slot $(echo $new | tr a-z A-Z) becomes the default; slot $(echo $old | tr a-z A-Z) the tryboot" >&2
zeval "$form"
