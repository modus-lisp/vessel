#!/usr/bin/env bash
# board-load.sh — install Lisp systems from local checkouts onto a running Zero, over SSH.
#
#   tools/board-load.sh DIR[:SYSTEM] ...
#
# Each DIR is a checkout with an .asd (SYSTEM defaults to the first defsystem in it).  It is
# tarred (no .git, no tests), cut into 16 KB pieces with an FNV-1a hash each, served over HTTP
# from the machine wired to the board, fetched by the board piece by piece (a piece that fails
# its hash is fetched again), and installed with MODUS-INT::INSTALL-TARBALL-FROM-BYTES -- the
# same route `kiln zero' uses for the desk.  Systems are installed in the order given.
#
# Env: ZERO_HOST (10.0.0.199), ZERO_GATEWAY (modus@modus-pi, the machine wired to the board),
#      ZERO_GATEWAY_IP (10.0.0.1, its address as the board sees it), ZERO_PORT (8765).
set -euo pipefail
ZH=${ZERO_HOST:-10.0.0.199} GW=${ZERO_GATEWAY:-modus@modus-pi} GIP=${ZERO_GATEWAY_IP:-10.0.0.1}
PORT=${ZERO_PORT:-8765}
SSHO=(-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=30 -o BatchMode=yes)
zeval() { ssh "${SSHO[@]}" -J "$GW" "modus@$ZH" "$1"; }   # one form; prints "= value"
[ $# -gt 0 ] || { sed -n '2,15p' "$0"; exit 2; }
work=$(mktemp -d)
# the HTTP server is killed by the PID it was started with, never by a name pattern
trap 'rm -rf "$work"; ssh "${SSHO[@]}" "$GW" "[ -f ~/board-load/pid ] && kill \$(cat ~/board-load/pid); rm -rf ~/board-load" >/dev/null 2>&1 || true' EXIT

# the fetch helpers, defined once per board session (cheap to redefine)
zeval '(progn (setq *jit-hot-only* nil)
 (defun kz-fnv (arr n) (let ((h 2166136261) (i 0)) (loop (when (>= i n) (return h)) (setq h (logand (* (logxor h (aref arr i)) 16777619) 4294967295)) (setq i (+ i 1)))))
 (defun kz-piece (url n hash) (let ((tries 0)) (loop (when (> tries 30) (return nil)) (let ((tb (modus-int::net-fetch-bytes url))) (when (and tb (= (cdr tb) n) (= (kz-fnv (car tb) n) hash)) (return (car tb)))) (setq tries (+ tries 1)))))
 (defun kz-fetch (base m) (let* ((total (let ((s 0)) (dolist (e m s) (setq s (+ s (cadr e)))))) (v (make-array total :element-type (quote (unsigned-byte 8)))) (off 0)) (dolist (e m v) (let ((p (kz-piece (concatenate (quote string) base (car e)) (cadr e) (caddr e)))) (when (null p) (return nil)) (replace v p :start1 off) (setq off (+ off (cadr e)))))))
 :helpers)' >/dev/null

ssh "${SSHO[@]}" "$GW" "rm -rf ~/board-load; mkdir -p ~/board-load; cd ~/board-load && { nohup python3 -m http.server $PORT --bind $GIP >http.log 2>&1 & echo \$! > pid; }"
for spec in "$@"; do
  dir=${spec%%:*} sys=""; [[ $spec == *:* ]] && sys=${spec#*:}
  dir=$(cd "$dir" && pwd); name=$(basename "$dir")
  if [ ! -f "$work/$name.lisp" ]; then   # a checkout named twice (two of its systems) is sent once
  (cd "$(dirname "$dir")" && tar cf "$work/$name.tar" --exclude=.git --exclude=test --exclude=tests \
     --exclude=t --exclude=inspect --exclude='*.fasl' "$name")
  python3 - "$work" "$name" <<'PY'
import os, sys
work, name = sys.argv[1], sys.argv[2]
data = open(os.path.join(work, name + ".tar"), "rb").read(); d = os.path.join(work, name); os.makedirs(d)
entries = []
for i in range(0, len(data), 16384):
    chunk = data[i:i+16384]; h = 2166136261
    for b in chunk: h = ((h ^ b) * 16777619) & 0xFFFFFFFF
    fn = "p%04d" % (i // 16384); open(os.path.join(d, fn), "wb").write(chunk)
    entries.append('("%s/%s" %d %d)' % (name, fn, len(chunk), h))
open(os.path.join(work, name + ".lisp"), "w").write("(" + " ".join(entries) + ")")
PY
  scp -q "${SSHO[@]}" -r "$work/$name" "$GW:board-load/"
  fi
  n=$(wc -c < "$work/$name.tar")
  echo "board-load: $name ($n bytes) ..." >&2
  sysarg=$([ -n "$sys" ] && echo "\"$sys\"" || echo "nil")
  zeval "(progn (setq modus-int::*tar-block-size* 512) (let ((tar (kz-fetch \"http://$GIP:$PORT/\" (quote $(cat "$work/$name.lisp"))))) (if tar (modus-int::install-tarball-from-bytes tar $sysarg) :fetch-failed)))"
done
