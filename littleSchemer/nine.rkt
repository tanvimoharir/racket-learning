#lang racket

(define get-elem
  (lambda (i num lat)
    (cond
      [(eq? i num) (car lat)]
      [else (get-elem (add1 i) num (cdr lat))])))

(define pick
  (lambda (a lat)
    (get-elem 1 a lat)))

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
    (eternity (x))))
;most partial function ever

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

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

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
      [(a-pair? (first pora)) (align (shift pora))]
      [else (build (first pora)
                   (align (second pora)))])))
;seems to be total function but how?

(define len*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (len* (first pora)) (len* (second pora)))])))

(define wei*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (*(wei* (first pora))2)
               (wei* (second pora)))])))
;didnt get this

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
      [else (build (first pora)
                   (shuffle (second pora)))])));not total

(define x1 '(a (b c)))
(define x2 '(a b))
(define x '((a b) (c d)))

(define one?
  (lambda (x)
    (eq? 1 x)))

(define C
  (lambda (n)
    (cond
      [(one? n) 1]
      [else
       (cond
         [(even? n) (C (quotient n 2))]
         [else (C (add1 (* 3 n)))])])));not total -try 0

(define A
  (lambda (n m)
    (cond
      [(zero? n) (add1 m)]
      [(zero? m) (A (sub1 n) 1)]
      [else (A (sub1 n)
               (A n (sub1 m)))])));total but dont know (A 4 3)

;can we write a func which tells us whether
;another func will return a value for all args

(define will-stop?
  (lambda (f)
    1))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

(define len
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (len (cdr l)))])))

(lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 (eternity (cdr l)))]));without define

;relacing len func for '() with length0

(define length0
  (lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 (eternity (cdr l)))])))

(lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 (length0 (cdr l)))]))

;for length<=1
(lambda (l)
  (cond
    [(null? l) 0]
    [else
     (add1
      ((lambda (l)
         (cond
           [(null? l) 0]
           [else (add1 eternity (cdr l))])))
      (cdr l))]))

;for 2 or fewer items
(lambda (l)
  (cond
    [(null? l) 0]
    [else
     (add1
      ((lambda (l)
         (cond
           [(null? l) 0]
           [else
            (add1
             ((lambda (l)
                (cond
                  [(null? l) 0]
                  [else
                   (add1
                    (eternity (cdr l)))])))
             (cdr l))])))
      (cdr l))]));cant keep writing infinite func

;need a func that looks like length
((lambda (length)
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
 eternity)

;((lambda (f)
;   (lambda (l)
;     (cond
;       [(null? l) 0]
;       [else (add1 (f (cdr l)))])))
;((lambda (g)
;   (lambda (l)
;     (cond
;       [(null? l) 0]
;       [else (add1 (g (cdr l)))])))))

;length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;length<=1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;length<=2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;length<=3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
     (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;passing mk-length to itself
;length0

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1
              (mk-length (cdr l)))]))))
;length<=1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1
              ((mk-length eternity) (cdr l)))]))))


;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   ((lambda (length)
;      (lambda (l)
;        (cond
;          [(null? l) 0]
;          [else (add1 (length (cdr l)))])))
;      (mk-length mk-length))))
; ) going out of memory

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else
        (add1
         ((lambda (x)
            ((mk-length mk-length) x))
          (cdr l)))]))))

(lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))

;y-combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))