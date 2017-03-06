(= *model* :vic-20+xk)
(defvar *pulse-interval* #x20)
(defvar *pulse-short* #x30)
(defvar *pulse-long* (+ *pulse-short* *pulse-interval*))
(defvar *tape-pulse* nil)

(defun c2nbit (o x)
  (write-byte (+ *pulse-short* (* (/ *pulse-interval* 4) (- 3 x))) o))

(defun c2ntap (o i)
  (write-dword #x8000000 o)
  (adotimes 64
    (c2nbit o 0))
  (c2nbit o 3)
  (awhile (read-byte i)
         nil
    (c2nbit o (bit-and (>> ! 6) 3))
    (c2nbit o (bit-and (>> ! 4) 3))
    (c2nbit o (bit-and (>> ! 2) 3))
    (c2nbit o (bit-and ! 3)))
  (adotimes 16
    (write-byte *pulse-short* o)))

(apply #'assemble-files "c2nwarp.prg"
       '("zeropage.asm"
         "bender/vic-20/basic-loader.asm"
         "loader.asm"))
(make-vice-commands "loader.vice.txt" "break .stop")

(format t "Short pulse: ~A~%" *pulse-short*)
(format t "Long pulse: ~A~%" *pulse-long*)
(format t "Pulse interval: ~A~%" *pulse-interval*)
(format t "Pulse subinterval: ~A~%" (/ *pulse-interval* 4))

(with-output-file o "sssa.tap"
  (write-tap o
    (+ (bin2cbmtap (cddr (string-list (fetch-file "c2nwarp.prg")))
                   "C2NWARP LOADER"
                   :start #x1001)
       (with-input-file i "sssa.exo.prg"
         (with-string-stream s (c2ntap s i))))))

(with-input-file i "sssa.tap"
  (with-output-file o "sssa.wav"
    (tap2wav i o 44100 (cpu-cycles :pal))))
(quit)
