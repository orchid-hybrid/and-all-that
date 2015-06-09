(define-library (match sxorder)

  (import (scheme base)
  	  (scheme cxr)
	  (srfi-13)) ; for string<

  (export atom<
  	  s<
  	  s<=
  	  s>
	  s>=)

  (include "sxorder.scm"))
