(define (main)
  (call-with-input-file "test.txt"
    (lambda (p)
      (tokenize p))))


;; num
;; num
;; (0 1 2 3)

;; start
;; num
;; (0 1 2 3)

;; start
;; ((empty) (epsilon) (empty) (empty))
;; (+)

;; start
;; ((empty) (empty) (epsilon) (empty))
;; (*)

;; start
;; ((empty) (empty) (empty) (epsilon))
;; (x y)

;; hand written from the graph
;;
;; (define (token state c)
;;   (case state
;;     ((start)
;;      (case c
;;        ((#\0 #\1 #\2 #\3) 'num)
;;        ((#\+) 'plus)
;;        ((#\*) 'times)
;;        ((#\x #\y) 'var)
;;        (else #f)))
;;     ((num)
;;      (case c
;;        ((#\0 #\1 #\2 #\3) 'num)
;;        (else #f)))
;;     ((plus)
;;      (case c
;;        (else #f)))
;;     ((times)
;;      (case c
;;        (else #f)))
;;     ((var)
;;      (case c
;;        (else #f)))))

;; (define (accepting? state)
;;   (case state
;;     ((num) number-token)
;;     ((plus) plus-token)
;;     ((times) times-token)
;;     ((var) var-token)
;;     (else #f)))

;; machine generated
;;

(define (token state char) (case state ((2) (case char ((#\0 #\1 #\2 #\3) '1) ((#\+) '3) ((#\*) '4) ((#\x #\y) '5) (else #f))) ((1) (case char ((#\0 #\1 #\2 #\3) '1) (else #f))) (else #f))) (define (accepting? state) (case state ((1) number-token) ((2) #f) ((3) plus-token) ((4) times-token) ((5) var-token) (else #f)))

(define start-state 2)

(define (number-token start len tok)
  (print (list 'number tok)))

(define (plus-token start len tok)
  (print (list 'plus tok)))

(define (times-token start len tok)
  (print (list 'times tok)))

(define (var-token start len tok)
  (print (list 'var tok)))

(define (print p) (display p) (newline))

;;;;

(define (tokenize p)
  (let loop ((state start-state)
	     (i 0)
	     (l 0)
	     (t '()))
    (cond
     ((eof-object? (peek-char p))
      (print 'done))
     ((token state (peek-char p)) =>
      (lambda (state)
	(loop state i (+ l 1) (cons (read-char p) t))))
     ((accepting? state) =>
      (lambda (emitter)
	(emitter i l (list->string (reverse t)))
	(loop start-state l 0 '())))
     (else (error "no parsable token")))))


