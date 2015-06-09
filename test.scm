(import (scheme base)
	(scheme write) (match match) (time) (srfi 1))

;; (define (f x)
;;   (match x
;;     ((a y) y)
;;     ((b (k)) 'b-k)
;;     ((b e) (list 'b e 'b))
;;     ((b e e) (list 'b e '=))
;;     ((b e x) (list 'b e '=/=))))

(define-rewrite-system f
  ((a) --> (b))
  ((b) --> (a))
  ((f x x) --> (f))
  ((f x y) --> (f! x y)))

(let-values (((s r) (f (read))))
  (display s)
  (newline)
  (display r)
  (newline))
