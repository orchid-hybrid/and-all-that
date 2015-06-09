(define-library (match compile-pattern)

  (import (scheme base)
  (scheme write)
          (match box)
	  (match trie))

  (export compile-pattern compile-quasipattern
  	  compile-patterns)

  (include "compile-pattern.scm"))
