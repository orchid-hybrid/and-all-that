(import (scheme base)
        (scheme write)
        (match match)
	(t regex)
	(graph))
;; rlwrap sagittarius -L. -S.sld regex-compiler.scm

(define r1 '(seq (symbol #\1) (seq (kleene (symbol #\0)) (symbol #\1))))

;(display (regex-simplify (regex '(d #\c (kleene (symbol #\c))))))
;(display (regex-simplify '(or (epsilon) (empty))))


(define (derivatives r alphabet)
  (filter (lambda (entry)
	    (not (equal? '(empty) (cdr entry))))
	  (map (lambda (c) (cons c (regex-simplify (regex `(d ,c ,r)))))
	       alphabet)))

(define (compile-regex r alphabet)
  (let loop ((g empty-graph)
	     (work-list (list r)))
    ;(print "tick")
    (if (null? work-list)
	g
	(let* ((r (car work-list))
	       (work-list (cdr work-list))
	       (r-next (derivatives r alphabet))
	       (g (foldr (lambda (edge g)
			   ;(print (list 'insert r (cdr edge)))
			   (let-values (((g found-b)
					 (insert-edge g
						      r
						      (cdr edge)
						      (car edge))))
			     '(if found-b
				 (print (list 'found (cdr edge)))
				 (print (list 'not-found (cdr edge)))
				 )
			     (unless found-b
				     (set! work-list (cons (cdr edge) work-list)))
			     g))
			 g r-next)))
	  (loop g work-list)))))

(define (foldr kons knil l)
  (if (null? l) knil (kons (car l) (foldr kons knil (cdr l)))))

(define (print p) (display p) (newline))

(for-each (lambda (entry)
	    (print (car entry))
	    (print (cadr entry))
	    (print (caddr entry))
	    (newline))
	  (compile-regex r1 (list #\0 #\1)))

