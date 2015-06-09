(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system f
  ((d x x) --> (one))
  ((d x y) where (symbol? y) --> (zero))
  ((d x (+ u v)) --> (+ (d x u) (d x v)))
  ((d x (* u v)) --> (+ (* u (d x v))
			(* (d x u) v))))

(display (f '(d x (* x x))))
(newline)
