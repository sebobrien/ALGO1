#lang racket


(define (occurences str pat)
  (define str-len (string-length str))
  (define pat-len (string-length pat))
  (define end (- str-len pat-len -1))
  (define (substring-starts-with i)
    (string=? (substring str i (+ i pat-len)) pat))
  (do (
       (i 0 (+ i 1))
       (match #f (substring-starts-with i))
       (count 0 (+ count (if match 1 0))))
    ((= i end) count)))

 (define (match-wild-simple t p)
   (define n-t (string-length t))
   (define n-p (string-length p))
   (let loop
     ((i-t 0)
      (i-p 0))
     (cond
       ((> i-p (- n-p 1))
        i-t)
       ((> i-t (- n-t n-p))
        #f)
       ((eq? #\* (string-ref p i-p))
         (loop i-t (+ i-p 1)))  
       ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
        (loop i-t (+ i-p 1)))
       (else
        (loop (+ i-t 1) 0)))))

;(display (match-wild-simple "helterskelter" "elte*skel"))

(define (find s wc)
  (let loop ((i 0))
      (cond
      (( = i (string-length s)) -1)
      (( eq? (string-ref s i) wc) i)
      (else
       (loop (+ i 1))
      ))))

(define (split-string s wc)
  (define i (find s wc))
  (if ( >= i 0)
  (cons (substring s 0 i)(substring s (+ i 1)))
  (cons s "") 
  ))
  
;(display (find "helter*kelter" #\*))
;(display (split-string  "helter*kelter" #\*))

(define (match-wild t p)
  (define ss (split-string p #\*))
  (let* ((left (car ss))
        (right(cdr ss))
        (i (match-wild-simple t left))
        (j (match-wild-simple t right)))
    (cons left right)    
    )
    )

(display (match-wild "helter*kelter" "helterskelter"))

 