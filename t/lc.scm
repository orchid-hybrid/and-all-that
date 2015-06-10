(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system lc
  ((eval (quote x) _) --> x)

  ((eval (var a) (env (nil))) --> (error))
  ((eval (var a) (env (c (b a v) e))) --> v)
  ((eval (var a) (env (c _ r))) --> (eval (var v) (env r)))

  ((eval (lam p b) (env e)) --> (clo e p b))

  ((eval (app f a) e) --> (app1 (eval f e) (eval a e)))
  ((app1 (clo ce p b) a) --> (eval b (env (c (b p a) ce)))))

(display (lc '(eval (var (a)) (env (nil)))))
(newline)
(display (lc '(eval (lam (x) (var (x))) (env (nil)))))
(newline)
(display (lc '(eval (app (lam (x) (var (x))) (quote (q))) (env (nil)))))
(newline)
