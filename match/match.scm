(define-syntax define-rewrite-system
  (syntax-rules ()
    ((define-rewrite-system <name> <clause> ...)
     (define <name>
       (letrec ((reduce (lambda (t)
                          (let ((s #t))
                            (let ((r (match/rewrite t s () <clause> ...)))
                              (if s
                                  (reduce* r)
                                  (values r s))))))
                (reduce* (lambda (t)
                           (cond
                            ((list? t) (reduce (cons (car t) (map reduce* (cdr t)))))
                            (else (reduce t))))))
         reduce*)))))

(define-syntax match/rewrite
  (syntax-rules (where -->)
    ((match/rewrite t s <m> (<pat> --> <body>) <clause> ...)
     (match/rewrite t s ((<pat> #t <body>) . <m>) <clause> ...))
    ((match/rewrite t s <m> (<pat> where <guard> --> <body>) <clause> ...)
     (match/rewrite t s ((<pat> <guard> <body>) . <m>) <clause> ...))
    ((match/rewrite t s (<m> ...))
     (match t <m> ... (else (set! s #f) t)))))

(define-syntax match
  (syntax-rules (else)
    ((match t (<pat> <guard> <body>) ... (else <else> ...))
     (let ((stack (list t)))
       (match-expander (<pat> ...) (<guard> ...) (<body> ...) stack (begin <else> ...))))
    ((match t (<pat> <guard> <body>) ...)
     (match t (<pat> <guard> <body>) ... (else (error "pattern match fell through"))))))

(define-syntax match-expander
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((pats (car (cdr form)))
	   (guards (cadr (cdr form)))
	   (results (caddr (cdr form)))
	   (stack (cadddr (cdr form)))
	   (fail (cadddr (cddr form)))
	   (%interpret-tree (rename 'interpret-tree)))
       ;; change this to `'(,
       ;; if you want to debug
       `(,%interpret-tree ()
                          ,(compile-patterns pats guards results)
                          ,stack
                          ,fail)))))
