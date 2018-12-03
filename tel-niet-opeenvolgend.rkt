#!r6rs

(import (rnrs base(6))
        (rnrs io simple(6))
        (prefix (a-d stack linked) s:))

(define (tel-niet-opeenvolgend l)
  (let loop ((i 0)
             (current l)
             (stack (s:new)))
    (if(null? current)
        i
        (if (or (s:empty? stack) (not (equal? (s:top stack) (car current))))
               (loop (+ i 1)
                     (cdr current)
                     (s:push! stack (car current)))
               (begin
                 (s:pop! stack)
                 (loop (- i 1)
                     (cdr current)
                     stack))))))

;(define (syso out)
;    (display out)(newline))
;(define l1 '("a" "b" "b" "c" "a" ) )
;(define l2 '("a" "b" "b" "a" ) )
;(define l3 '("a" "b" "c" "d" "d" "c" "b" ) )
;(syso (tel-niet-opeenvolgend l1))
;(syso (tel-niet-opeenvolgend l2))
;(syso (tel-niet-opeenvolgend l3))
