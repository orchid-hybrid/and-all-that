(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system f
  ((+ x (z)) --> x)
  ((+ x (s y)) --> (s (+ x y))))

(display (f '(+ (s (z))
		(s (s (z))))))
(newline)
