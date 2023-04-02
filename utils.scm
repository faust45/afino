
(define (repeat n f)
  (let loop ((c 0))
    (when (< c n)
      (f)
      (loop (+ 1 c)))))

;; (display
;;  (tree-il->scheme
;;   (macroexpand
;;    '(->> msg
;; 	 (cons extra)
;; 	 send))))
