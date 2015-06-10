(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system f
  ((c (n x) (c (*) (c (n y) rest))) --> (c (n (* x y)) rest))
  ((c (n x) (c (+) (c (n y) rest))) --> (c (n (+ x y)) rest))
  ((c (open) (c (n x) (c (close) rest))) --> (c (n x) rest)))

(define (prepare-example l)
  (define (prepare s)
    (if (number? s)
	`(n ,s)
	(list s)))
  (if (null? l)
      '(nil)
      `(c ,(prepare (car l)) ,(prepare-example (cdr l)))))

(define examples
  (map prepare-example (list '(1 + 2 * 3)
			     '(2 * 3 + 4)
			     '(2 * open 3 + 4 close))))

(for-each (lambda (example)
	    (display (list example '--> (f example)))
	    (newline))
	  examples)
