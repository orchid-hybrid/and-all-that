;; The syntax of patterns is:
;;
;; <pat>  ::= <var> | (<atom> <pat> ...)

(define (var? s) (symbol? s))
(define (functor? s) (and (list? s) (symbol? (car s))))

;; matching instructions are:
;;
;; <m> ::= (bind <var>)
;;       | (compare-equal? <form>)
;;       | (decons)

(define (compile-pattern box pat)
;  (display pat) (newline)
  (cond ((var? pat) (compile-var box pat))
	((functor? pat) (compile-functor box (car pat) (cdr pat)))
	(else (error "Invalid pattern"))))


(define (compile-var box var)
  (if (member var (unbox box))
      (list `(compare-equal? ,var))
      (begin
	(set-box! box (cons var (unbox box)))
	(list `(bind ,var)))))


(define (compile-functor box name args)
  (append `((decons)
	    (compare-equal? ',name))
	  (compile-args box args)))

(define (compile-args box args)
  (if (null? args)
      `((compare-equal? '()))
      (let ((a (compile-pattern box (car args))))
	(append (cons '(decons) a)
		(compile-args box (cdr args))))))


(define (compile-patterns patterns guards bodies)
  (merge (map (lambda (pattern guard body)
		(append (compile-pattern (make-box '()) pattern) (list `(guard ,guard) `(execute ,body))))
	      patterns guards bodies)))
