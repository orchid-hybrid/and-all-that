(import (scheme base)
	(scheme write)
	(match match))

(define-rewrite-system lc
  ((eval (var a) (env (c (b a v) e))) --> v)
  ((eval (var a) (env (c _ r))) --> (eval (var v) (env r)))
  ((eval (var a) (env (nil))) --> (error))

  ((eval (lam p b) (env e)) --> (clo e p b))

  ((eval (app (clo e p b) a) (env _)) --> (eval b (env (c (b p a) e)))))

(display (lc '(eval (var (a)) (env (nil)))))
(newline)
(display (lc '(eval (lam (x) (var (x))) (env (nil)))))
(newline)
(display (lc '(eval (app (lam (x) (var (x))) (q)) (env (nil)))))
(newline)
