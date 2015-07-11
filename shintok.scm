(import (scheme base)
        (scheme write)
        (match match)
        (t regex)
        (graph))
;; rlwrap sagittarius -L. -S.sld regvec-compiler.scm

(define number-class
  '(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define hex-class
  '(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
       #\A #\B #\C #\D #\E #\F))

(define alpha-class
  '(or #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
       #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define reserved-symbol-class
  '(or #\' #\, #\. #\; #\` #\# #\" #\( #\)))

(define symbol-class
  '(or #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~))

(define whitespace-class
  '(or #\space #\newline))

(define alphabet
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\' #\, #\. #\; #\` #\# #\" #\( #\)
    #\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\{ #\} #\~
    #\space #\newline))


;; syntax tokens

(define open-token #\()
(define close-token #\))
(define dot-token #\.)
(define quote-mark-token #\')
(define quasiquote-mark-token #\`)
(define unquote-mark-token #\,)

;; atoms

(define symbol-token
  `(seq (or ,alpha-class ,symbol-class)
        (star (or ,alpha-class ,symbol-class ,number-class))))

(define number-token
  `(or (plus ,number-class)
       (seq #\# #\b (plus (or #\0 #\1)))
       (seq #\# #\x (plus ,hex-class))))

(define boolean-token
  `(seq #\# (or #\t #\f)))

(define character-token
  `(seq #\# #\\
        (or ,number-class ,alpha-class ,symbol-class
            ,reserved-symbol-class)
        (star (or ,number-class ,alpha-class ,symbol-class))))

(define string-token
  `(seq #\"
        (star (or (or . ,(delete #\\ (delete #\" alphabet)))
                  (seq #\\ (or #\" #\\))))
        #\"))

;; whitespace

(define comment
  `(seq #\; (kleene (or . ,(delete #\newline alphabet)))
        #\newline))
(define whitespace-token
  `(plus (or #\space #\newline ,comment)))



(define token-names
  '(open-token close-token dot-token quote-mark-token quasiquote-mark-token unquote-mark-token
               symbol-token number-token boolean-token character-token string-token
               whitespace-token))


(define (foldr kons knil l)
  (if (null? l) knil (kons (car l) (foldr kons knil (cdr l)))))

(define (tidy r)

  (define (tidy* h) (if (eq? h 'star) 'kleene h))
  (define (make-thing h l)
    (if (equal? 'kleene h)
        `(kleene . ,l)
        (foldr (lambda (a b)
                 (if b
                     (list h a b)
                     a))
               #f
               l)))
  (cond

   ((char? r) `(symbol ,r))
   ((and (list? r) (eq? (car r) 'plus))
    (let ((r0 (tidy (cadr r))))
      `(seq ,r0 (kleene ,r0))))
   ((list? r)
    (let ((res (make-thing (tidy* (car r)) (map tidy (cdr r)))))
      (unless (or (eq? 'or (car r))
                  (eq? 'seq (car r)))
              '(print (list 'tidied- res)))
      res))
   (else r)))


(define tokens
  (map tidy
       (list open-token close-token dot-token quote-mark-token quasiquote-mark-token unquote-mark-token
             symbol-token number-token boolean-token character-token string-token
             whitespace-token)))






;(display (regex-simplify (regex '(d #\c (kleene (symbol #\c))))))
;(display (regex-simplify '(or (epsilon) (empty))))

;; (display (regex-simplify (regex '(or (kleene (or (symbol 0) (or (symbol 1) (or (symbol 2) (symbol 3))))) (seq (v1r (false)) (kleene (or (symbol 0) (or (symbol 1) (or (symbol 2) (symbol 3))))))))))
;; (newline)

;; (display (regex-simplify (regex '(v1r (false)))))
;; (newline)

;; (display (regex-simplify (regex '(seq (v1r (false)) (epsilon)))))
;; (newline)

                                        ;(input y (or (kleene (or (symbol 0) (or (symbol 1) (or (symbol 2) (symbol 3))))) (seq (v1r (false)) (kleene (or (symbol 0) (or (symbol 1) (or (symbol 2) (symbol 3))))))) (empty))


;(print (regex `(d #\0 (seq (or (symbol #\0) (or (symbol #\1) (or (symbol #\2) (or (symbol #\3) (empty))))) (kleene (or (symbol #\0) (or (symbol #\1) (or (symbol #\2) (or (symbol #\3) (empty))))))))))
;(exit)

(define (derivatives rs alphabet)
  (filter (lambda (entry)
            (not (every (lambda (re) (equal? '(empty) re))
                        (cdr entry))))
          (map (lambda (c) (cons c (map (lambda (r)
                                          ;(print (list 'input c)) (write r)
                                          ;(newline)
                                        ;(print (list 'output (regex-simplify (d c r))))
                                          ;(print "simpstart")
                                          ;(print r)
                                          (let ((s
                                                 (regex-simplify (d c r))))
                                            ;(print s)
                                            ;(print "simpend")
                                            
                                            s))
                                        rs)))
               alphabet)))

(define (every p l) (if (null? l) #t (and (p (car l)) (every p (cdr l)))))

(define (compile-regex rs alphabet)
  (let loop ((g empty-graph)
             (work-list (list rs)))
    ;(print "tick")
    (if (null? work-list)
        g
        (let* ((rs (car work-list))
               (work-list (cdr work-list))
               ;(p0 (print "p0"))
               (rs-next (derivatives rs alphabet))
               ;(p0 (print "p1"))
               (g (foldr (lambda (edge g)
                           ;(print (list 'insert r (cdr edge)))
                           (let-values (((g found-b)
                                         (insert-edge g
                                                      rs
                                                      (cdr edge)
                                                      (car edge))))
                             '(if found-b
                                 (print (list 'found (cdr edge)))
                                 (print (list 'not-found (cdr edge)))
                                 )
                             (unless found-b
                                     (set! work-list (cons (cdr edge) work-list)))
                             g))
                         g rs-next))
               ;(p2 (print "p2"))
               )
          (loop g work-list)))))


(define (print p) (display p) (newline))

;; (for-each (lambda (entry)
;;          (print (car entry))
;;          (print (cadr entry))
;;          (print (caddr entry))
;;          (newline))
;;        (compile-regex (list r-num r-plus r-times r-var)
;;                       alphabet))

(define sym-table '())
(define counter 0)
(define (gensym object)
  (cond ((assoc object sym-table) => cdr)
        (else
         (set! counter (+ counter 1))
         (set! sym-table (cons (cons object counter) sym-table))
         counter)))

(define (grouper-insert key thing groups)
  (if (null? groups)
      (list (cons key (list thing)))
      (if (equal? key (car (car groups)))
          (cons (cons key (cons thing (cdr (car groups))))
                (cdr groups))
          (cons (car groups)
                (grouper-insert key thing (cdr groups))))))

(define (compile-tokenizer token-names regvec alphabet)
  (define (thing l1 l2)
    (if (null? l1)
        #f
        (if (car l2)
            (car l1)
            (thing (cdr l1) (cdr l2)))))
  (print (list 'start-is (gensym regvec)))
  (let* ((graph (compile-regex regvec alphabet))
         ;(t1 (print "t1)"))
         (graph (map (lambda (entry)
                       (list (gensym (car entry))
                             (gensym (cadr entry))
                             (caddr entry)))
                     graph))
         ;(t1 (print "t2)"))
         (states (reverse (map cdr sym-table)))
         ;(t1 (print "t3)"))
         (accepting (map (lambda (entry)
                           (cons (cdr entry)
                                 (map v (car entry))))
                         sym-table))
         ;(t1 (print "t4)"))
         (grouped-graph (foldr (lambda (a g)
                                 (grouper-insert (car a) (cdr a) g))
                               '()
                               graph)))
    `((define (token state char)
        (case state .
              ,(append (map (lambda (group)
                              (let ((state (car group)))
                                `((,state) (case char .
                                                 ,(append (map (lambda (entry)
                                                                 `(,(cadr entry) ',(car entry)))
                                                               (cdr group))
                                                          (list `(else #f)))))))
                            grouped-graph)
                       (list `(else #f)))))
      (define (accepting? state)
        (case state .
          ,(append (map (lambda (state)
                          `((,state) ,(thing token-names
                                             (cdr (assoc state accepting)))))
                        states)
                   (list '(else #f))))))))

(write (compile-tokenizer token-names
                          tokens
                          alphabet))

;; ((empty) (empty) (empty) (epsilon))
;; ((empty) (empty) (empty) (epsilon))
;; (0 1 2 3 + * x y)

;; ((empty) (empty) (epsilon) (empty))
;; ((empty) (empty) (epsilon) (empty))
;; (0 1 2 3 + * x y)

;; ((empty) (epsilon) (empty) (empty))
;; ((empty) (epsilon) (empty) (empty))
;; (0 1 2 3 + * x y)

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





'(seq (or (or (symbol a)
             (or (symbol t) (symbol f)))
         (symbol -))
     (seq (or (or (symbol a)
                  (or (symbol t) (symbol f)))
              (or (symbol -)
                  (or (symbol 0) (symbol 1))))
          (or (or (symbol a)
                  (or (symbol t) (symbol f)))
              (or (symbol -)
                  (or (symbol 0) (symbol 1))))))


'(write (regex-simplify
        '(seq (or (or (symbol a)
                      (or (symbol t) (symbol f)))
                  (symbol -))
              (seq (or (or (symbol a) (or (symbol t) (symbol f))) (or (symbol -) (or (symbol 0) (symbol 1)))) (or (or (symbol a) (or (symbol t) (symbol f))) (or (symbol -) (or (symbol 0) (symbol 1))))))))


'(let* ((r1 (tidy comment))
       (r2a (d #\; r1))
       (r2 (regex-simplify r2a)))
  (print r1) (newline)
  (print r2a) (newline)
  (print r2) (newline)
  (print (regex-simplify (d #\f r2))))

