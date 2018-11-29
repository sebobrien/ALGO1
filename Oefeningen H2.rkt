#!r6rs

(import (rnrs))

(define (print s) (display s)(newline))

(print "Oef 1")
(print (char->integer #\a))

; Extra oefening
(define (ascii-value->number ascii-value)
  123
  )

(print "Oef 2")
(define (my-string->number s)
  (do (
       (i 0 (+ i 1))
       (res 0 (+ -48 (char->integer (string-ref s i)) (* 10 res))))
    ((= i (string-length s)) res)))
(print (my-string->number "12345"))
; Worst/best/average case: O(n) (n length of s)
; Worst/best/average case: O(log(n)) (n value of number)

(print "Oef 3")
; v is a prefix of t
; w is a suffix or t
; The length of w is 3. This is also denoted as |w| = 3
; Is v a proper prefix of t? Yes

(print "Oef 4")
; Prefixes: "", "H", "He", "Hel", "Hell", "Hello"
; Suffixes: "", "o", "lo", "llo", "ello", "Hello"
; Proper P: Prefixes without "Hello" and ""
; Proper S: Suffixes without "Hello" and ""

(print "Oef 5")
; (string string -> number | #f)

(print "Oef 6")
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
(print (occurences "bababxzy" "ba"))

(print "Oef 7")
(define (match-wildcard t p)
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
      ((or
        (eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
        (eq? (string-ref p i-p) "."))
       (loop i-t (+ i-p 1)))
      (else
       (loop (+ i-t 1) 0)))))
(print (match-wildcard "helterskelter" "hel...skel"))
