(define-library (match match)

  (import (scheme base)
  	  (scheme cxr)
          (rnrs eval)
	  (sagittarius) ;; for er-macro-transformer
  	  (match trie)
	  (match compile-pattern)
	  (match interpret-tree))

  (export define-rewrite-system)

  (include "match.scm"))
