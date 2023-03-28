#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define lookingN
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? (car lat) a) #t]
      [else (lookingN a (cdr lat))]))) ;this doesnt seem to be the
;definition pf looking

(define get-index
  (lambda (n i lat)
    (cond
      [(null? lat) '()]
      [(eq? i n) (car lat)]
      [else (get-index n (add1 i) (cdr lat))])))

(define pick
  (lambda (n lat)
    (get-index n 1 lat)))


(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(number? sorn) (keep-looking a (pick sorn lat) lat)]
      [else (eq? sorn a)])))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
    

(define lat '(6 2 4 caviar 5 7 3))

(define eternity
  (lambda (x)
    (eternity x)));most partial function ever

(define first
  (lambda (lat)
    (car lat)))

(define second
  (lambda (lat)
    (car (cdr lat))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora)) (align (shift pora))];*
      [else (build (first pora) (align (second pora)))])))
;*violating the 7th commandemnt not reducing the lat with recursin


(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora)) (length* (second pora)))])))


(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))])))

(define revpair
  (lambda (pair)
    (cond
      [(null? pair) (quote())]
      [else (build (second pair) (first pair))])))

(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora)) (shuffle (revpair pora))]
      [else (build (first pora) (shuffle (second pora)))])))
; shuffle is not total
; try with case '((a b) (c d))

(define one?
  (lambda (n)
    (eq? 1 n)))

(define C
  (lambda (n)
    (cond
      [(one? n) 1]
      [else
       (cond
         [(even? n) (C (quotient n 2))]
         [else (C (add1 (* 3 n)))])])))
;not total check 0


(define A
  (lambda (n m)
    (cond
      [(zero? n) (add1 m)]
      [(zero? m) (A (sub1 n) 1)]
      [else  (A (sub1 n) (A n (sub1 m)))])))
;ackerman function
;a's arguments like shuffle and looking do not decrease for recursion
;A is total but (A 4 3)?

;(define last-try
;  (lambda (x)
;    (and (will-stop? last-try) (eternity x))))

((lambda (len)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (len (cdr l)))])))
 eternity)
  