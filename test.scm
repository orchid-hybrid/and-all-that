(import (scheme base)
	(scheme write) (match match) (time) (srfi 1))

(define (f x)
  (match x
    ((a y) y)
    ((b (k)) 'b-k)
    ((b e) (list 'b e 'b))
    ((b e e) (list 'b e '=))
    ((b e x) (list 'b e '=/=))))

(display (f (read)))
(newline)
