(= *model* :vic-20+xk)
(var *pulse-interval* #x60)
(var *pulse-short* #x1c)
(var *pulse-long* (+ *pulse-short* *pulse-interval*))
(var *tape-pulse* (* 8 (+ *pulse-short* (half *pulse-interval*))))
(var *path-main* "arukanoido/arukanoido.prg") ;"sssa.exo.prg")
(var *title* "ARUKANOIDO")
(var *show-map?* nil)

(fn c2nbit (o x)
  (write-byte (+ *pulse-short* (* (/ *pulse-interval* 4) (- 3 x))) o))

(fn c2n-leader (o)
  (adotimes 64
    (write-byte *pulse-short* o))
  (write-byte *pulse-long* o)
  (write-byte *pulse-short* o))

(fn c2n-trailer (o)
  (adotimes 32
    (write-byte *pulse-short* o)))

(fn c2n-refs (o)
  (dotimes (i 256)
    (adotimes 4
      (c2nbit o (bit-xor ! (bit-and (- 256 i) 3))))))

(fn c2ntap (o i)
  (write-dword #x8000000 o)
  (c2n-leader o)
  (c2n-refs o)
  (adotimes 384
    (write-byte *pulse-short* o)
    (write-byte *pulse-long* o))
  (c2n-leader o)
  (awhile (read-byte i)
         nil
    (c2nbit o (bit-and (>> ! 6) 3))
    (c2nbit o (bit-and (>> ! 4) 3))
    (c2nbit o (bit-and (>> ! 2) 3))
    (c2nbit o (bit-and ! 3)))
  (c2n-trailer o))

(apply #'assemble-files "c2nwarp.prg"
       '("zeropage.asm"
         "bender/vic-20/basic-loader.asm"
         "loader.asm"))
(make-vice-commands "loader.vice.txt" "break .stop")

(format t "Short pulse: ~A~%" *pulse-short*)
(format t "Long pulse: ~A~%" *pulse-long*)
(format t "Pulse interval: ~A~%" *pulse-interval*)
(format t "Pulse subinterval: ~A~%" (/ *pulse-interval* 4))
(format t "Pulse rate: ~A~%" (integer (/ (cpu-cycles :pal) *tape-pulse*)))
(format t "C2NWARP rate: ~A~%" (integer (* 2 (/ (cpu-cycles :pal) *tape-pulse*))))

(with-output-file o "arukanoido/arukanoido.tap"
  (write-tap o
    (+ (bin2cbmtap (cddr (string-list (fetch-file "c2nwarp.prg")))
                   *title*
                   :start #x1201
                   :short-data? t)
       (with-input-file i *path-main*
         (with-string-stream s (c2ntap s i))))))
;       (bin2cbmtap (cddr (string-list (fetch-file *path-main*)))
;                   *title*
;                   :start #x1201
;                   :short-leader? t
;                   :short-data? t))))

(with-input-file i "arukanoido/arukanoido.tap" (with-output-file o "arukanoido/arukanoido.pal.wav" (tap2wav i o 44100 (cpu-cycles :pal))))
(with-input-file i "arukanoido/arukanoido.tap" (with-output-file o "arukanoido/arukanoido.ntsc.wav" (tap2wav i o 44100 (cpu-cycles :ntsc))))

(sb-ext:run-program "/usr/bin/zip" (list "-r" "-9" "arukanoido.zip" "arukanoido")
                    :pty cl:*standard-output*)
(quit)
