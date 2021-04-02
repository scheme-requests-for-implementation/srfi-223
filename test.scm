(import (scheme base))
(import (scheme division))
(import (scheme inexact))

(import (chibi test))

(include "srfi-223.scm")


;; OEIS A003056
(define (nth-inverted-triangular n)
  ;; The OEIS formula yields the sequence 0, 1, 2, 2, 3, 3, 3, ...
  ;; instead of 0, 1, 1, 2, 2, 2, 3, 3, 3, 3 ..., so this is
  ;; implemented in terms of A002024, which makes it correct.
  (- (floor (+ 1/2 (exact (sqrt (* 2 (+ n 1)))))) 1))
(define (inverted-triangular-ref _ n) (nth-inverted-triangular n))

(test-group "Generalized bisection procedures"
  (test "left bisection for n for the nth inverted triangular number ≥ x."
        3 (bisect-left #f 2 inverted-triangular-ref < 0 100))
  (test "right bisection for n for the nth inverted triangular number < x."
        6 (bisect-right #f 2 inverted-triangular-ref < 0 100)))

(test-group "Bisections over vectors"
  (test "left bisection over a vector"
        1 (vector-bisect-left #(1 2 2 3 5) 2 <))
  (test "right bisection over a vector"
        3 (vector-bisect-right #(1 2 2 3 5) 2 <)))

(test-group "Bisections over vectors of things other than numbers"
  (test "left bisection over a vector of characters"
        1 (vector-bisect-left #(#\A #\Z #\a #\z) #\B char<?))
  (test "right bisection over a vector of characters"
        2 (vector-bisect-right #(#\A #\Z #\a #\z) #\Z char<?)))

;; Example of a vector with negative indexes: the mvector accessor for
;; a vector guarantees that index 0 will always refer to the median
;; value in the vector, provided the vector is sorted.
(define-record-type <mvector>
  (vector->mvector vector)
  mvector?
  (vector mvector-vector mvector-vector-set!))

(define (mvector . vals)
  (vector->mvector (apply vector vals)))

(define (mvector-size mvector) (vector-length (mvector-vector mvector)))
(define (mvector-first-idx mvector)
  (- (floor-quotient (mvector-size mvector) 2)))
(define (mvector-last-idx mvector)
  (- (ceiling-quotient (mvector-size mvector) 2)
     1))
(define (mvector-ref mvector idx)
  (vector-ref (mvector-vector mvector)
              (+ (abs (mvector-first-idx mvector)) idx)))

(define-values (mvector-bisect-left mvector-bisect-right)
  (bisection mvector-ref
             (lambda (mv)
               (values (mvector-first-idx mv)
                       (+ 1 (mvector-last-idx mv))))))

(define mvtest-even (mvector 1 1 2 3 5 8 13 21))
(define mvtest-odd (mvector 1 1 2 3 5 8 13 21 34))

(test-group "Implementation of example sequence type with negative indexes"
  (test "first-idx of even-sized mvector" -4 (mvector-first-idx mvtest-even))
  (test "first-idx of odd-sized mvector" -4 (mvector-first-idx mvtest-odd))

  (test "last-idx of even-size mvector" 3 (mvector-last-idx mvtest-even))
  (test "last-idx of odd-size mvector" 4 (mvector-last-idx mvtest-odd))

  (test "min value of even-sized mvector"
        1 (mvector-ref mvtest-even (mvector-first-idx mvtest-even)))
  (test "min value of odd-sized mvector"
        1 (mvector-ref mvtest-odd (mvector-first-idx mvtest-odd)))

  (test "max value of even-sized mvector"
        21 (mvector-ref mvtest-even (mvector-last-idx mvtest-even)))
  (test "max value of odd-sized mvector"
        34 (mvector-ref mvtest-odd (mvector-last-idx mvtest-odd)))

  (test "median value of odd-sized mvector"
        5 (mvector-ref mvtest-odd 0))
  (test "median value of even-sized mvector"
        5 (mvector-ref mvtest-even 0)))

(test-group "Bisection of sequence type with negative indexes"
  (test "left bisection returning negative result"
        -2 (mvector-bisect-left mvtest-odd 2 <))
  (test "right bisection returning negative result"
        -2 (mvector-bisect-right mvtest-odd 2 <))

  (test "left bisection returning positive result"
        1 (mvector-bisect-left mvtest-odd 8 <))
  (test "right bisection returning positive result"
        2 (mvector-bisect-right mvtest-odd 9 <)))
