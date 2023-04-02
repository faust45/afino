(define-module (macros)
  #:export (->
	    ->>
	    x->
	    x->>
	    do-while
	    do-until
	    repeat))


(define-syntax ->
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f . (f-rest ...))) (f x f-rest ...))
    ((_ x f) (f x))
    ((_ x (f . (f-rest ...)) rest ...) (-> (f x f-rest ...) rest ...))
    ((_ x f rest ...) (-> (f x) rest ...))))

(define-syntax ->>
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f ...)) (f ... x))
    ((_ x f) `(f x))
    ((_ x (f ...) rest ...) (->> (f ... x) rest ...))
    ((_ x f rest ...) (->> (f x) rest ...))))

(define-syntax x->>
  (syntax-rules ()
    ((_) #f)
    ((_ rest ...) (lambda (x) (->> x rest ...)))))

(define-syntax x->
  (syntax-rules ()
    ((_) #f)
    ((_ rest ...) (lambda (x) (-> x rest ...)))))

(define-syntax-rule (repeat n body)
  (let loop ((i 0))
    (when (< i n)
      body
      (loop (+ 1 i)))))

(define-syntax-rule (do-while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))

(define-syntax-rule (do-until cond body ...)
  (do-while (not cond) body ...))
