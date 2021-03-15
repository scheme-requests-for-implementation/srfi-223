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

(define-syntax define-bisection
  (syntax-rules ()
    ((_ bisect-left-name bisect-right-name ref-proc lo-hi-proc)
     (begin
       (define bisect-left-name
         (case-lambda
          ((a val less?)
           (let-values (((lo hi) (lo-hi-proc a)))
             (bisect-left a val ref-proc less? lo hi)))
          ((a val less? lo hi)
           (bisect-left a val ref-proc less? lo hi))))
       (define bisect-right-name
         (case-lambda
          ((a val less?)
           (let-values (((lo hi) (lo-hi-proc a)))
             (bisect-right a val ref-proc less? lo hi)))
          ((a val less? lo hi)
           (bisect-right a val ref-proc less? lo hi))))))
    ((_ bisect-left-name bisect-right-name ref-proc)
     (define-bisection bisect-left-name bisect-right-name
       ref-proc
       (lambda (a) (error "both lo and hi arguments must be given to this procedure"))))))

(define-bisection vector-bisect-left vector-bisect-right
  vector-ref (lambda (v) (values 0 (vector-length v))))
