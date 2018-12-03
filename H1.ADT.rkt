#!r6rs

(library
 (fraction)
 (export new fraction? numerator denominator + * / -)
 (import
  (prefix (rnrs base) io:)
  (rename (rnrs base)
          (numerator base:numerator)
          (denominator base:denominator)
          (+ base:+)
          (* base:*)
          (/ base:/)
          (- base:-)))

(define fraction-tag 'fraction)

(define (new num den)
  (let ((gcd-num-dem (gcd num den)))
  (list fraction-tag (base:/ num gcd-num-dem) (base:/ den gcd-num-dem))))

(define (numerator f)
  (list-ref f 1))

(define (denominator f)
  (list-ref f 2))

(define (fraction? v)
 (eq? (car v) 'fraction ))

(define (+ f1 f2)
  (if (eq? (denominator f1) (denominator f2))
      (new (base:+ (numerator f1) (numerator f2)) (denominator f1) )      
       (+
        (new (base:* (numerator f1) (denominator f2)) (base:* (denominator f1)(denominator f2)))
        (new (base:* (numerator f2) (denominator f1)) (base:* (denominator f1)(denominator f2))) 
      )))

(define (- f1 f2)
  (if (eq? (denominator f1) (denominator f2))
      (new (base:- (numerator f1) (numerator f2)) (denominator f1) )      
       (-
        (new (base:* (numerator f1) (denominator f2)) (base:* (denominator f1)(denominator f2)))
        (new (base:* (numerator f2) (denominator f1)) (base:* (denominator f1)(denominator f2))) 
      )))
  

(define (* f1 f2)
  (new (base:* (numerator f1) (numerator f2)) (base:* (denominator f1) (denominator f2)))
  )

(define (/ f1 f2)
  (new (base:* (numerator f1) (denominator f2)) (base:* (denominator f1) (numerator f2)))
  )


(define f1 (new 2 3))
(define f2 (new 1 2))


)

