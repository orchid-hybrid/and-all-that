(define empty-graph '())

(define (insert-edge g a b e)
  ;; inserts a new edge from a to b with arc e
  ;;
  ;; returns two value:
  ;; 1. the new graph
  ;; 2. #t is b already existed in the graph, #f if not
  (let-values (((found-a-b found-b) (search-graph g a b)))
    (values (if found-a-b
		(insert-edge-arc g a b e)
		(cons (list a b (list e)) g))
	    found-b))
  
  (values (insert-edge-helper g a b e)
	  (find (lambda (entry) (equal? b (cadr entry))))))

(define (search-graph g a b)
  ;; search through the graph to check if there is already an edge for a-b
  ;; and check if the terminal node b already exists
  ;;
  ;; returns two values
  ;; 1. found a-b?
  ;; 2. found b?
  (let loop ((found-b #f) (g* g))
    (if (null? g*)
	(values #f #t)
	(let ((entry (car g*)))
	  (if (equal? b (cadr entry))
	      (if (equal? a (car entry))
		  (values #t #t)
		  (loop #t (cdr g*)))
	      (loop found-b (cdr g*)))))))

(define (insert-edge-arc g a b e)
  (if (null? g)
      (list (list a b (list e)))
      (let ((entry (car g)))
	(if (and (equal? a (car entry))
		 (equal? b (cadr entry)))
	    (cons (list a b (cons e (caddr entry))) (cdr g))
	    (cons entry (insert-edge-arc (cdr g) a b e))))))
