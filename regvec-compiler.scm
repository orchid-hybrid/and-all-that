(import (scheme base)
        (scheme write)
        (match match)
	(t regex)
	(graph))
;; rlwrap sagittarius -L. -S.sld regvec-compiler.scm

(define r-dig '(or (symbol #\0)
		   (or (symbol #\1)
		       (or (symbol #\2)
			   (or (symbol #\3)
			       (empty))))))
(define r-num `(seq ,r-dig (kleene ,r-dig)))

(define r-plus `(symbol #\+))

(define r-times `(symbol #\*))

(define r-var `(or (symbol #\x) (symbol #\y)))

(define alphabet (list #\0 #\1 #\2 #\3 #\+ #\* #\x #\y))

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
					  (regex-simplify (d c r)))
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
	       (rs-next (derivatives rs alphabet))
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
			 g rs-next)))
	  (loop g work-list)))))

(define (foldr kons knil l)
  (if (null? l) knil (kons (car l) (foldr kons knil (cdr l)))))

(define (print p) (display p) (newline))

;; (for-each (lambda (entry)
;; 	    (print (car entry))
;; 	    (print (cadr entry))
;; 	    (print (caddr entry))
;; 	    (newline))
;; 	  (compile-regex (list r-num r-plus r-times r-var)
;; 			 alphabet))

(define sym-table '())
(define counter 0)
(define (gensym object)
  (print "g")
  (cond ((assoc object sym-table) => cdr)
	(else
	 (set! counter (+ counter 1))
	 (set! sym-table (cons (cons object counter) sym-table))
	 counter)))

(define (grouper-insert key thing groups)
  (print "i")
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
  (print "t")
  (let* ((graph (compile-regex regvec alphabet))
	 (debugt1  (print "t1"))
	 (graph (map (lambda (entry)
		       (list (gensym (car entry))
			     (gensym (cadr entry))
			     (caddr entry)))
		     graph))
	 (debugt2  (print "t2"))
	 (states (reverse (map cdr sym-table)))
	 (debugt3  (print "t3"))
	 (accepting (map (lambda (entry)
			   (cons (cdr entry)
				 (map v (car entry))))
			 sym-table))
	 (debugt4  (print "t4"))
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
		   (list '(else #f)))))))

  (write (compile-tokenizer '(num plus times var)
			    (list r-num r-plus r-times r-var)
			    alphabet)))

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

