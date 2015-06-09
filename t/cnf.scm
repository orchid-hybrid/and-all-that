(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system cnf
;  ((and a a) --> a
;  ((or a a) --> a)
  ((not (not x)) --> x)
  ((not (and a b)) --> (or (not a) (not b)))
  ((not (or a b)) --> (and (not a) (not b)))
  ((or (and a b) c) --> (and (or a c) (or b c)))
  ((or a (and b c)) --> (and (or a b) (or a c))))

(define e1 '(not (or b c)))
(define e2 '(or (and a b) c))
(define e3 '(and a (or b (and d e))))

(define (test f)
  (begin
    (display `(,f -> ,(cnf f)))
    (newline)))

(map (lambda (x) (test x)) `(,e1 ,e2 ,e3))