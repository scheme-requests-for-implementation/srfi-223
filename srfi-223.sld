(define-library (srfi-223)
  (import (scheme base)
          (scheme case-lambda))

  (export bisect-left bisect-right
          define-bisection
          vector-bisect-left vector-bisect-right)

  (include "srfi-223.scm"))
