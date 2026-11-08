;;;; zero-console.lisp — the desk's Console: the board's serial console in a window, and a line
;;;; to type into it.
;;;;
;;;; Everything the board writes to its serial port is also kept in RAM, dmesg style (modus's
;;;; serial ring: MODUS-INT::DMESG, SERIAL-RING-COUNT, SERIAL-RING-BYTES), and this window shows
;;;; it as it is written: the boot, the network, a sync's progress, anything a program prints to
;;;; the console.  The bottom line takes a Lisp form; Enter evaluates it on the desk's thread, as
;;;; the Terminal does, and the form and what it printed and returned are written to the serial
;;;; console -- so they appear here, on the wire, and in (dmesg) alike.
;;;;
;;;; Keys: Backspace, Up/Down through what was entered, Enter.  A form left open (an unbalanced
;;;; parenthesis) continues on the next line.
;;;;
;;;; An image without the ring (anything but the Pi CL image) has no Console.

(defun %kiln-ring-fn (name)
  (let ((s (find-symbol name "MODUS-INT")))
    (and s (fboundp s) (symbol-function s))))

(defun kiln-console-available-p ()
  (and (%kiln-ring-fn "SERIAL-RING-COUNT") (%kiln-ring-fn "SERIAL-RING-BYTES")
       (%kiln-ring-fn "SERIAL-RING-BOOT-START") t))

(defun %kiln-console-say (string)
  "STRING and a newline to the serial console (so to the ring, and to every Console)."
  (%kz-say string))

(defun %kiln-console-app (fb)
  "The Console window (see the file's header).  The desk's app contract: (values on-key
   on-pointer tick copy-p close text-p)."
  (let* ((font (or *kiln-mono-font* (glass:default-font)))
         (count-fn (%kiln-ring-fn "SERIAL-RING-COUNT"))
         (bytes-fn (%kiln-ring-fn "SERIAL-RING-BYTES"))
         (tm nil) (seen 0) (last-byte 0)
         (input "") (pending "") (history '()) (hpos -1))
    (labels ((cp (c) (glass-term::feed-cp tm c))
             (feed-bytes (v)
               ;; the console writes LF alone; the grid wants CR LF
               (loop for b across v
                     do (when (and (= b 10) (/= last-byte 13)) (cp 13))
                        (unless (= b 0) (glass-term::feed-byte tm b))
                        (setf last-byte b)))
             (prompt-text () (if (string= pending "") "> " ". "))
             (erase-line () (cp 13) (cp 27) (cp (char-code #\[)) (cp (char-code #\2)) (cp (char-code #\K)))
             (draw-input ()
               (erase-line)
               (loop for ch across (concatenate 'string (prompt-text) input) do (cp (char-code ch))))
             (take-new ()
               ;; what the console wrote since the last look, the input line kept below it
               (let ((now (funcall count-fn)))
                 (unless (= now seen)
                   (erase-line)
                   (unless (= last-byte 10) (cp 13) (cp 10) (setf last-byte 10))
                   (feed-bytes (funcall bytes-fn seen now))
                   (unless (= last-byte 10) (cp 13) (cp 10) (setf last-byte 10))
                   (setf seen now)
                   (draw-input))))
             (enter ()
               (let ((all (concatenate 'string pending input)))
                 (%kiln-console-say (concatenate 'string (prompt-text) input))
                 (setf input "")
                 (if (%kiln-balanced-p all)
                     (progn
                       (setf pending "" hpos -1)
                       (unless (string= (string-trim " " all) "")
                         (push all history)
                         (dolist (l (%kiln-terminal-eval all)) (%kiln-console-say l))))
                     (setf pending (concatenate 'string all (string #\Newline))))
                 (take-new)
                 (draw-input)))
             (key (ks)
               (let ((ch (%kiln-keysym-char ks)))
                 (cond ((member ks '(10 13)) (return-from key (enter)))  ; the Zero's keyboard
                       ((member ks '(8 127))                ; sends some keys as control codes
                        (when (plusp (length input)) (setf input (subseq input 0 (1- (length input))))))
                       ((< 0 ks 32) nil)
                       ((= ks #xff08)                       ; BackSpace
                        (when (plusp (length input)) (setf input (subseq input 0 (1- (length input))))))
                       ((= ks #xff52)                       ; Up: the previous entry
                        (when (< (1+ hpos) (length history))
                          (incf hpos) (setf input (nth hpos history))))
                       ((= ks #xff54)                       ; Down
                        (if (plusp hpos)
                            (progn (decf hpos) (setf input (nth hpos history)))
                            (setf hpos -1 input "")))
                       ((eql ch #\Newline) (return-from key (enter)))
                       (ch (setf input (concatenate 'string input (string ch))))))
               (draw-input)))
      (setf tm (glass-term:make-terminal :cols 20 :rows 4 :ppem 12 :font font :nerd-font nil
                                         :pty (lambda (s) (declare (ignore s)) nil)))
      (glass-term:resize-terminal-px tm (glass:fb-width fb) (glass:fb-height fb))
      (setf (glass-term:terminal-fb tm) fb
            (glass-term::terminal-drawn-chars tm) nil)
      (glass:fb-fill fb (glass-term::pal 0))
      ;; start with the last few KB of the console, not all 64 KB of it
      (let* ((now (funcall count-fn))
             (boot (funcall (%kiln-ring-fn "SERIAL-RING-BOOT-START"))))
        (setf seen (logand (- now (min 6000 (logand (- now boot) #xFFFFFFFF))) #xFFFFFFFF)))
      (take-new)
      (draw-input)
      (values (lambda (down ks) (when down (key ks)))
              nil
              (lambda ()
                (take-new)
                (glass-term::render tm)
                (glass-term:terminal-take-dirty tm))
              nil
              (lambda () nil)
              t))))
