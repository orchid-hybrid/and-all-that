(import (scheme base)
        (scheme write)
        (match match))

(define-rewrite-system regex
  ((v (empty)) --> (false))
  ((v (epsilon)) --> (true))
  ((v (symbol _)) --> (true))
  ((v (seq r s)) --> (and (v r) (v s)))
  ((v (kleene r)) --> (false))
  ((v (or r s)) --> (or (v r) (v s)))

  ((v1 r) --> (v1r (v r)))
  ((v1r (true)) --> (epsilon))
  ((v1r (false)) --> (empty))

  ((d a (empty)) --> (empty))
  ((d a (epsilon)) --> (empty))
  ((d a (symbol a1)) where (eq? a a1) --> (epsilon))
  ((d a (symbol _)) --> (empty))
  ((d a (seq r s)) --> (or (seq (d a r) s) (seq (v1 r) (d a s))))
  ((d a (kleene r)) --> (seq (d a r) (kleene r)))
  ((d a (or r s)) --> (or (d a r) (d a s))))

(define-rewrite-system regex-simp
  ((kleene (kleene r)) --> (kleene r))
  ((or (epsilon) (seq r (kleene r))) --> (kleene r))
  ((or (seq r (kleene r)) (epsilon)) --> (kleene r))
  ((seq (empty) r) --> (empty))
  ((seq r (empty)) --> (empty))
  ((seq (epsilon) r) --> r)
  ((seq r (epsilon)) --> r)
  ((or (empty) r) --> r)
  ((or r (empty)) --> r)
  ((or r r) --> r)
  ((seq (seq a b) c) --> (seq a (seq b c)))
  ((or (or a b) c) --> (or a (or b c)))
  ((seq (or r1 r2) r) --> (or (seq r1 r) (seq r2 r)))
  ((seq r (or r1 r2)) --> (or (seq r r1) (seq r r2))))


(display (regex '(d #\c (kleene (symbol #\c)))))
