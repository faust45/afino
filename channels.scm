(define-module (channels)
  #:export (make-channel
	    ch-write
	    ch-read
	    foo
	    %foo-local))

(use-modules (ice-9 match)
	     (ice-9 atomic)
	     (ice-9 threads)
	     (ice-9 futures)
	     (ice-9 control))

(define (make-channel)
  (cons (make-atomic-box (list))
	(make-atomic-box (list))))

(define ch-messages car)
(define ch-listeners cdr)

(define (atomic-list-append! atom item)
  (let ((ref (atomic-box-ref atom)))
    (or
     (eq? ref
	  (atomic-box-compare-and-swap!
	   atom
	   ref
	   (append ref (list item))))
     (atomic-list-append! atom item))))

(define (atomic-list-drop-first! atom)
  (let ((ref (atomic-box-ref atom)))
    (match ref
      ((item rest ...)
       (if (eq? ref
		(atomic-box-compare-and-swap!
		 atom
		 ref
		 rest))
	   item
	   (atom-list-drop-first! atom)))
      (_ #nil))))

(define (ch-read ch)
  (or (atomic-list-drop-first! (ch-messages ch))
      (if (suspendable-continuation? (default-prompt-tag))
	  (abort
	   (lambda (k)
	     (atomic-list-append! (ch-listeners ch) k)))
	  (raise-exception 'not-inside-suspendable-continuation))))

(define worker
  (begin-thread
   (while #t
     (sleep 1000))))

(define (run-worker promt msg)
  (system-async-mark
   (lambda ()
     (% (promt msg)))
   worker))

(define (ch-write ch msg)
  (atomic-list-append! (ch-messages ch) msg)
  (match (atomic-list-drop-first! (ch-listeners ch))
    (promt
     (let ((msg (atomic-list-drop-first! (ch-messages ch))))
       (run-worker promt msg)))
    (_ #nil)))


