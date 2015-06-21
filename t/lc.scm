(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system lc
  ((eval (quote x) _) --> x)
  ((eval (int x) _) --> (int x))

  ((eval (add x y) e) --> (add% (eval x e) (eval y e)))
  ((add% (int x) (int y)) --> (scheme (+ x y)))

  ((eval (var a) (nil)) --> (error))
  ((eval (var a) (c (c a v) e)) --> v)
  ((eval (var a) (c _ r)) --> (eval (var v) r))

  ((eval (lam p b) e) --> (clo e p b))

  ((eval (app f a) e) --> (app% (eval f e) (eval a e)))
  ((app% (clo ce p b) a) --> (eval b (c (c p a) ce)))
  
  ((eval (equal? a b) e) --> (equal?% (eval a e) (eval b e)))
  ((equal?% a a) --> (true))
  ((equal?% a b) where (not (equal? a b)) --> (false))

  ((eval (if t c a) e) --> (eval (if% (eval t e) c a) e))
  ((if% (true) c a) --> c)
  ((if% (false) c a) --> a))

(display (lc '(eval (var (a)) (nil))))
;; (error)
(newline)
(display (lc '(eval (lam (x) (var (x))) (nil))))
;; (clo (nil) (x) (var (x)))
(newline)
(display (lc '(eval (app (lam (x) (var (x))) '(q)) (nil))))
;; (q)
(newline)
(display (lc '(eval (app (lam (f) (app (var (f)) '(q))) (lam (x) (var (x)))) (nil))))
;; (q)
(newline)
(display (lc '(eval (add (int 1) (int 2)) (nil))))
;; => 3
(newline)
(display (lc '(eval (app (lam (x) (add (var (x)) (int 2))) (int 1)) (nil))))
;; => 3
(newline)
(display (lc '(eval (if (equal? '(q) '(q)) '(a) '(b)) (nil))))
;; => (a)
(newline)
