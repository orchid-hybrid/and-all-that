(define (atom? x) (or (null? x) (symbol? x)))

(define (atom< x y)
  (if (null? x)
      (not (null? y))
      (string< (symbol->string x)
	       (symbol->string y))))

(define (s< x y)
  (cond
   ((and (atom? x) (atom? y)) (atom< x y))
   ((and (atom? x) (pair? y)) #t)
   ((and (pair? x) (atom? y)) #f)
   ((and (pair? x) (pair? y))
    (if (s< (car x) (car y))
	#t
	(if (equal? (car x) (car y))
	    (s< (cdr x) (cdr y))
	    #f)))))

