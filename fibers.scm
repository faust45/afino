(add-to-load-path "./")

(define-module (fibers)
  #:export (make-channel
	    make-channel-fanout
	    ch-filter
	    ch-write
	    ch-read
	    ch-opened?
	    ch-closed?
	    ch-close
	    log
	    fiber))

(use-modules (ice-9 match)
	     (ice-9 exceptions)
	     (ice-9 q)
	     (srfi srfi-9)
	     (srfi srfi-1)
	     (ice-9 control)
	     (ice-9 threads)
	     (macros))

(define-record-type <channel>
  (%make-channel mutex listeners messages is-opened is-fanout)
  channel?
  (mutex ch-mutex)
  (listeners ch-listeners ch-set-listeners)
  (messages ch-messages)
  (is-opened ch-opened? ch-set-is-opened)
  (is-fanout ch-fanout?))

(define (make-channel)
  (%make-channel (make-mutex) (make-q) (make-q) #t #f))

(define (make-channel-fanout)
  (%make-channel (make-mutex) (make-q) (make-q) #t #t))

(define ch-closed?
  (x-> ch-opened? not))

(define (ch-close ch)
  (ch-set-is-opened ch #f))

(define task-queue (make-q))
(define task-queue-mutex
  (make-mutex))

(define worker-wakeup-flag
  (make-condition-variable))

(define logger
  (open-file "./fibers-log" "a0"))

(define (log msg)
  (display msg logger)
  (newline logger))

(define worker #nil)
(define (start-worker)
  (begin-thread
   (while #t
     (lock-mutex task-queue-mutex)
     (when (q-empty? task-queue)
       (wait-condition-variable worker-wakeup-flag task-queue-mutex))
     (match (deq! task-queue)
       ((task . msg)
	(unlock-mutex task-queue-mutex)
	(with-exception-handler
	    (lambda (ex)
	      (log ex))
	  (lambda () (% (task msg)))))
       (_ (unlock-mutex task-queue-mutex))))))

(define (wakeup-worker)
  (if worker
      (signal-condition-variable worker-wakeup-flag)
      (set! worker (start-worker))))

(define (try-process-msg ch)
  (when (and (not (q-empty? (ch-messages ch)))
	     (not (q-empty? (ch-listeners ch))))
    (let* ((msg (deq! (ch-messages ch)))
	   (add-task (lambda ()
		       (enq! task-queue
			     (cons (deq! (ch-listeners ch)) msg)))))
      (with-mutex task-queue-mutex
	(if (ch-fanout? ch)
	    (do-until (q-empty? (ch-listeners ch))
		      (add-task))
	    (add-task))))
    (wakeup-worker)))

(define (ch-write ch msg)
  (with-mutex (ch-mutex ch)
    (if (ch-closed? ch)
	(raise-continuable (make-exception 'err-write-to-closed-channel)))
    (enq! (ch-messages ch) msg)
    (try-process-msg ch)))

;; Blocking read (from empty channel),
;; possible only inside promt (fiber runs thunk inside promt)
(define (ch-read ch)
  (with-mutex (ch-mutex ch)
    (if (ch-closed? ch)
	(raise-continuable (make-exception 'err-read-from-closed-channel)))
    (if (q-empty? (ch-messages ch))
	(abort (x->> (enq! (ch-listeners ch))))
	(deq! (ch-messages ch)))))

(define (ch-filter cond src)
  (let ((dst (make-channel)))
    (fiber
     (do-while
      (ch-opened? dst)
      (let ((msg (ch-read src)))
	(if (and (list? msg)
		 (equal? (assv (car cond) msg) cond)) 
	    (ch-write dst msg)))))
    dst))

(define-syntax-rule (fiber body ...)
  (let ((task (cons (lambda (msg) body ...)
		    'start-fiber)))
    (with-mutex task-queue-mutex
      (enq! task-queue task))
    (wakeup-worker)))
