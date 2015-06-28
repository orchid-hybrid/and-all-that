(import (scheme base)
        (scheme write)
        (match match)
	(t regex)
	(graph))

(display (regex-simplify (regex '(d #\c (kleene (symbol #\c))))))
