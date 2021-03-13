(define-library (bisect)
  (import (scheme base)
          (scheme case-lambda))

  (export bisect-left bisect-right
          vector-bisect-left vector-bisect-right
          bytevector-bisect-left bytevector-bisect-right)

  (load "bisect.scm"))
