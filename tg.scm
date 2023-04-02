(add-to-load-path "./")

(use-modules(ice-9 format)
	     (ice-9 match)
	     (ice-9 pretty-print)
	     (sxml simple)
	     (fibers)
	     (language tree-il)
	     (ice-9 threads)
	     (macros)
	     (json)
	     (ice-9 rdelim)
             (system foreign)
	     (system foreign-library))

(define tdlib-ffi
  (load-foreign-library "libtdjson"
			#:search-path '("/home/moon/.guix-profile/lib/")))

(define td-create-client-id
  (foreign-library-function tdlib-ffi "td_create_client_id"
			    #:return-type int))

(define td-send
  (foreign-library-function tdlib-ffi "td_send"
			    #:arg-types (list int '*)))
(define td-execute
  (foreign-library-function tdlib-ffi "td_execute"
			    #:arg-types (list '*)))

(define td-receive
  (foreign-library-function tdlib-ffi "td_receive"
			    #:arg-types (list double)
			    #:return-type '*))

(define user-phone
  '(("@type" . "setAuthenticationPhoneNumber")
    ("phone_number" . "")))

(define auth-params
  '(
    ("@type" . "setTdlibParameters")
    ("api_id" . "")
    ("api_hash" . "")
    ("database_directory" . "/home/moon/tg-data")
    ("system_language_code" . "en")
    ("@database_encryption_key" . "demo")
    ("use_message_database" . #f)
    ("application_version" . "1.0")
    ("device_model" . "Desktop")))

(define td-client-id
  (td-create-client-id))

(define (send msg)
  (log "send msg: ")
  (log msg)
  (->> msg
       scm->json-string
       string->pointer
       (td-send td-client-id)))

(define (execute msg)
  (-> msg
      scm->json-string
      string->pointer
      td-execute))

(define (tg-init)
  (execute '(("@type" . "setLogVerbosityLevel")
	     ("new_verbosity_level" . "1")))
  (send auth-params))

(define server
  (make-channel))

;; (tg-init)

;; (define tg-msg-loop
;;   (begin-thread
;;    (while #t
;;      (let ((msg (td-receive 100)))
;;        (if (not (null-pointer? msg))
;; 	   (->> msg
;; 		pointer->string
;; 		json-string->scm
;; 		(cons 'tg-update)
;; 		(ch-write server)))))))


(define tg-msg-ch
  (make-channel))

(define ch
  (make-channel-fanout))

(define filter-a
  (ch-filter '(foo . a) ch))

(define filter-b
  (ch-filter '(foo . b) ch))

(fiber
 (do-while #t
	   (display (ch-read filter-a))
	   (display " in a ")
	   (newline)))

(fiber
 (do-while #t
	   (display (ch-read filter-b))
	   (display " in b")
	   (newline)))

(ch-write ch '((foo . a)))
(ch-write ch '((foo . b)))

(sleep 1)


;; (msg '(("@type" . "getChats")
;; 	       ("limit" . "20")
;; 	       ("@extra" . ,extra)))
;; (define (send-asynd msg)
;;   (let* ((extra ("@extra" . ,(random 1000)))
;; 	 (ch (ch-filter extra tg-msg-ch)))
;;     (send (cons extra msg))
;;     (log (ch-read ch))
;;     (ch-close ch)))

;; (define (send-sync msg)
;;   (let ((extra `("@extra" . ,(random 1000)))
;; 	(ch (make-channel)))
;;     (ch-write server (subscribe . (,ch . extra)))
;;     (->> msg (cons extra) (send))
;;     (ch-read ch)))
