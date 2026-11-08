#!/usr/bin/env bash
# slot-install.sh — put a kernel and core in the Zero's slot A, from a running board, over SSH.
#
#   tools/slot-install.sh KERNEL.img DESK.core
#
# The card has two slots (see the card's config.txt / tryboot.txt):
#   B, the DEFAULT: config.txt  -> kernel8-b.img, cmdline-b.txt, core 2:/boot/desk-b.core
#   A, tryboot only: tryboot.txt -> kernel8-a.img, cmdline-a.txt, core 2:/boot/desk-a.core
# This replaces slot A's kernel (boot partition) and core (the store, partition 2), and
# touches nothing of slot B's -- so whatever happens, a plain reset still boots B.  Boot A
# once with (modus-int::reboot :tryboot t) on the board.
#
# Each file is cut into 64 KB pieces with an FNV-1a hash each and served over HTTP from the
# machine wired to the board; the board fetches each piece (again, if its hash is wrong)
# straight into fs-fat's output handle, so nothing bigger than a piece is in memory.  The
# file is then read back from the card and its FNV-1a compared with the host's.  The piece
# list is fetched over HTTP too: an SSH exec of tens of KB never reaches the board's REPL.  Needs an
# fs-fat with FAT32-OPEN-OUTPUT on the board (tools/board-load.sh fs-fat if it has not).
#
# A board SSH eval dies after ~20 min with no output, so each write runs in one eval whose
# result is also kept in KZ-SLOT-LOG, which this polls.
#
# Env: ZERO_HOST, ZERO_GATEWAY, ZERO_GATEWAY_IP, ZERO_PORT as tools/board-load.sh.
set -euo pipefail
ZH=${ZERO_HOST:-10.0.0.199} GW=${ZERO_GATEWAY:-modus@modus-pi} GIP=${ZERO_GATEWAY_IP:-10.0.0.1}
PORT=${ZERO_PORT:-8766}
SSHO=(-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=30 -o BatchMode=yes)
zeval() { ssh "${SSHO[@]}" -J "$GW" "modus@$ZH" "$1"; }
[ $# -eq 2 ] || { sed -n '2,24p' "$0"; exit 2; }
kernel=$(realpath "$1") core=$(realpath "$2")
work=$(mktemp -d)
trap 'rm -rf "$work"; ssh "${SSHO[@]}" "$GW" "[ -f /tmp/slot-install/pid ] && kill \$(cat /tmp/slot-install/pid); rm -rf /tmp/slot-install" >/dev/null 2>&1 || true' EXIT

# piece NAME FILE: pieces in $work/NAME/, and $work/NAME.lisp = (TOTAL-FNV (PATH LEN FNV) ...)
piece() {
  python3 - "$work" "$1" "$2" <<'PY'
import os, sys
work, name, src = sys.argv[1:]
data = open(src, "rb").read(); d = os.path.join(work, name); os.makedirs(d)
def fnv(b, h=2166136261):
    for x in b: h = ((h ^ x) * 16777619) & 0xFFFFFFFF
    return h
entries = []
for i in range(0, len(data), 65536):
    chunk = data[i:i+65536]; fn = "p%05d" % (i // 65536)
    open(os.path.join(d, fn), "wb").write(chunk)
    entries.append('("%s/%s" %d %d)' % (name, fn, len(chunk), fnv(chunk)))
open(os.path.join(work, name + ".lisp"), "w").write("(%d %d %s)" % (len(data), fnv(data), " ".join(entries)))
PY
}
piece kernel "$kernel"; piece core "$core"
ssh "${SSHO[@]}" "$GW" "rm -rf /tmp/slot-install; mkdir -p /tmp/slot-install"
scp -q "${SSHO[@]}" -r "$work/kernel" "$work/core" "$work/kernel.lisp" "$work/core.lisp" "$GW:/tmp/slot-install/"
ssh "${SSHO[@]}" "$GW" "cd /tmp/slot-install && { nohup python3 -m http.server $PORT --bind $GIP >http.log 2>&1 & echo \$! > pid; }"

zeval '(progn (setq *jit-hot-only* nil)
 (defvar kz-slot-log nil)
 (defun kz-fnv (arr n &optional (h 2166136261)) (let ((i 0)) (loop (when (>= i n) (return h)) (setq h (logand (* (logxor h (aref arr i)) 16777619) 4294967295)) (setq i (+ i 1)))))
 (defun kz-piece (url n hash) (let ((tries 0)) (loop (when (> tries 30) (return nil)) (let ((tb (modus-int::net-fetch-bytes url))) (when (and tb (= (cdr tb) n) (= (kz-fnv (car tb) n) hash)) (return (car tb)))) (setq tries (+ tries 1)))))
 (defun kz-readback-fnv (v path) (let ((h (fs-fat:fat32-open-input v path)) (buf (make-array 65536 :element-type (quote (unsigned-byte 8)))) (off 0) (f 2166136261)) (when h (loop (let ((n (fs-fat:fat32-read-into h off buf))) (when (zerop n) (return (list off f))) (setq f (kz-fnv buf n f) off (+ off n)))))))
 (defun kz-manifest (url) (let ((tb (modus-int::net-fetch-bytes url))) (unless tb (error "slot: no manifest at ~a" url)) (let ((*read-eval* nil)) (read-from-string (map (quote string) (function code-char) (subseq (car tb) 0 (cdr tb)))))))
 (defun kz-slot-write (v path base name) (handler-case (kz-slot-write-1 v path base (kz-manifest (concatenate (quote string) base name ".lisp"))) (error (e) (let ((r (list path :error (princ-to-string e)))) (push r kz-slot-log) r))))
 (defun kz-slot-write-1 (v path base m) (let* ((t0 (get-universal-time)) (h (fs-fat:fat32-open-output v path)) (ok nil) (r nil))
   (unwind-protect (progn (dolist (e (cddr m)) (let ((p (kz-piece (concatenate (quote string) base (car e)) (cadr e) (caddr e)))) (unless p (error "slot: piece ~a failed" (car e))) (fs-fat:fat32-write-output h p :end (cadr e))))
                          (fs-fat:fat32-close-output h) (fs-fat:fat32-sync v) (setq ok t))
     (unless ok (fs-fat:fat32-close-output h :abort t)))
   (setq r (list path :want (list (car m) (cadr m)) :have (kz-readback-fnv v path) :seconds (- (get-universal-time) t0)))
   (push r kz-slot-log) r))
 :slot-helpers)'

run() {  # run LABEL FORM -- one write; poll the log if the eval's session drops
  local label=$1 form=$2 before
  before=$(zeval '(length kz-slot-log)' | tr -dc 0-9)
  echo "slot-install: $label ..." >&2
  timeout 3000 ssh "${SSHO[@]}" -J "$GW" "modus@$ZH" "$form" || true
  until [ "$(zeval '(length kz-slot-log)' | tr -dc 0-9)" -gt "$before" ]; do sleep 60; done
  zeval '(prin1-to-string (car kz-slot-log))'
}
U="http://$GIP:$PORT/"
# the core first: the store has room for old and new, and a failure leaves slot A's old core
# (SLOT_SKIP_CORE=1: the core is already in -- only the kernel)
[ -n "${SLOT_SKIP_CORE:-}" ] || {
run "core -> 2:/boot/desk-a.core ($(stat -c %s "$core") bytes)" \
  "(kz-slot-write (kiln-zero-card::%store) \"/boot/desk-a.core\" \"$U\" \"core\")"
zeval '(if (getf (cdr (car kz-slot-log)) :error) :failed :core-ok)' | grep -q CORE-OK || { echo "slot-install: the core did not go in; slot A's kernel is left alone" >&2; exit 1; }
}
# the boot partition has no room for two kernels: slot A's old one goes first
zeval '(let ((v (kiln-zero-card::%volume))) (when (fs-fat:fat32-lookup v "/kernel8-a.img") (fs-fat:fat32-delete v "/kernel8-a.img")) (fs-fat:fat32-sync v) :deleted)'
run "kernel -> 1:/kernel8-a.img ($(stat -c %s "$kernel") bytes)" \
  "(kz-slot-write (kiln-zero-card::%volume) \"/kernel8-a.img\" \"$U\" \"kernel\")"
zeval '(let ((bad (remove-if (lambda (r) (equal (getf (cdr r) :want) (getf (cdr r) :have))) (subseq kz-slot-log 0 (min 2 (length kz-slot-log)))))) (if bad (list :MISMATCH bad) :verified))'
