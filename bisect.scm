(define (bisect-left a val ref less? lo hi)
  (if (>= lo hi) lo
      (let ((mid (floor-quotient (+ lo hi) 2)))
        (if (less? (ref a mid) val)
            (bisect-left a val ref less? (+ mid 1) hi)
            (bisect-left a val ref less? lo mid)))))

(define (bisect-right a val ref less? lo hi)
  (if (>= lo hi) lo
      (let ((mid (floor-quotient (+ lo hi) 2)))
        (if (less? val (ref a mid))
            (bisect-left a val ref less? lo mid)
            (bisect-left a val ref less? (+ mid 1) hi)))))

(define vector-bisect-left
  (case-lambda
   ((a val less?)
    (vector-bisect-left a val less? 0 (vector-length a)))
   ((a val less? lo)
    (vector-bisect-left a val less? lo (vector-length a)))
   ((a val less? lo hi)
    (bisect-left a val vector-ref less? lo hi))))

(define vector-bisect-right
  (case-lambda
   ((a val less?)
    (vector-bisect-right a val less? 0 (vector-length a)))
   ((a val less? lo)
    (vector-bisect-right a val less? lo (vector-length a)))
   ((a val less? lo hi)
    (bisect-right a val vector-ref less? lo hi))))

(define bytevector-bisect-left
  (case-lambda
   ((a val)
    (bytevector-bisect-left a val 0 (bytevector-length a)))
   ((a val lo)
    (bytevector-bisect-left a val lo (bytevector-length a)))
   ((a val lo hi)
    (bisect-left a val bytevector-u8-ref < lo hi))))

(define bytevector-bisect-right
  (case-lambda
   ((a val)
    (bytevector-bisect-right a val 0 (bytevector-length a)))
   ((a val lo)
    (bytevector-bisect-right a val lo (bytevector-length a)))
   ((a val lo hi)
    (bisect-right a val bytevector-u8-ref < lo hi))))
