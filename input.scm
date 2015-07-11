(import (scheme base) (scheme file) (scheme write))

(define (main f)
  (call-with-input-file f
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

;; (define (token state char) (case state ((2) (case char ((#\0 #\1 #\2 #\3) '1) ((#\+) '3) ((#\*) '4) ((#\x #\y) '5) (else #f))) ((1) (case char ((#\0 #\1 #\2 #\3) '1) (else #f))) (else #f))) (define (accepting? state) (case state ((1) number-token) ((2) #f) ((3) plus-token) ((4) times-token) ((5) var-token) (else #f)))

;; (define start-state 2)

;; (define (number-token start len tok)
;;   (print (list 'number tok)))

;; (define (plus-token start len tok)
;;   (print (list 'plus tok)))

;; (define (times-token start len tok)
;;   (print (list 'times tok)))

;; (define (var-token start len tok)
;;   (print (list 'var tok)))

;; (define (print p) (display p) (newline))


(define (token state char) (case state ((1) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) '16) ((#\') '17) ((#\,) '18) ((#\.) '19) ((#\;) '3) ((#\`) '20) ((#\#) '14) ((#\") '6) ((#\() '21) ((#\)) '22) ((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~) '4) ((#\space #\newline) '2) (else #f))) ((16) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) '16) (else #f))) ((3) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\' #\, #\. #\; #\` #\# #\" #\( #\) #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~ #\space) '3) ((#\newline) '2) (else #f))) ((14) (case char ((#\b) '13) ((#\f #\t) '15) ((#\x) '11) ((#\\) '9) (else #f))) ((13) (case char ((#\0 #\1) '12) (else #f))) ((12) (case char ((#\0 #\1) '12) (else #f))) ((11) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F) '10) (else #f))) ((10) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F) '10) (else #f))) ((9) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\' #\, #\. #\; #\` #\# #\" #\( #\) #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~) '8) (else #f))) ((8) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~) '8) (else #f))) ((6) (case char ((#\") '7) ((#\\) '5) ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\' #\, #\. #\; #\` #\# #\( #\) #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\] #\^ #\_ #\{ #\} #\~ #\space #\newline) '6) (else #f))) ((5) (case char ((#\" #\\) '6) (else #f))) ((4) (case char ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~) '4) (else #f))) ((2) (case char ((#\;) '3) ((#\space #\newline) '2) (else #f))) (else #f))) (define (accepting? state) (case state ((1) #f) ((2) whitespace-token) ((3) #f) ((4) symbol-token) ((5) #f) ((6) #f) ((7) string-token) ((8) character-token) ((9) #f) ((10) number-token) ((11) #f) ((12) number-token) ((13) #f) ((14) #f) ((15) boolean-token) ((16) number-token) ((17) quote-mark-token) ((18) unquote-mark-token) ((19) dot-token) ((20) quasiquote-mark-token) ((21) open-token) ((22) close-token) (else #f)))



(define start-state 1)

(define (whitespace-token start len tok)
  '(print (list 'whitespace tok))
  )

(define (number-token start len tok)
  (print (list 'number tok)))

(define (symbol-token start len tok)
  (print (list 'symbol tok)))

(define (boolean-token start len tok)
  (print (list 'boolean tok)))

(define (character-token start len tok)
  (print (list 'character tok)))

(define (string-token start len tok)
  (print (list 'string tok)))

(define (number-token start len tok)
  (print (list 'number tok)))

(define (quote-mark-token start len tok)
  (print (list 'quote-mark tok)))

(define (unquote-mark-token start len tok)
  (print (list 'unquote-mark tok)))

(define (quasiquote-mark-token start len tok)
  (print (list 'quasiquote-mark tok)))

(define (dot-token start len tok)
  (print (list 'dot tok)))

(define (open-token start len tok)
  (print (list 'open tok)))

(define (close-token start len tok)
  (print (list 'close tok)))

(define (print p) (display p) (newline))



;;;;

(define (tokenize p)
  (let loop ((state start-state)
             (i 0)
             (l 0)
             (t '()))
    ;(display (list state 'peeking (peek-char p))) (newline)
    (cond
     ((equal? (peek-char p) #\tab)
      (error "mate.."))
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


