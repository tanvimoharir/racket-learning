#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (if (number? x) (+ 1 x) (error))))

(define sub1
  (lambda (x)
    (- x 1)))

(define zero?
  (lambda (x)
    (eq? 0 x)))

(define o+
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (o+ (add1 x) (sub1 y))])))

(define o-
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (o- (sub1 x) (sub1 y))])))

(define tup?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(number? (car lat)) (tup? (cdr lat))]
      [else #f])))

(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else (o+ (car tup) (addtup (cdr tup)))])))

(define o*
  (lambda (x y)
    (cond
      [(zero? y) 0]
      [else (o+ x (o* x (sub1 y)))])))

(define tup+
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(null? l2) l1]
      [else (cons (+ (car l1) (car l2)) (tup+ (cdr l1) (cdr l2)))])))

(define o>
  (lambda (m n)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (o> (sub1 m) (sub1 n))])))

(define o<
  (lambda (x y)
    (cond
      [(zero? y) #f]
      [(zero? x) #t]
      [else (o< (sub1 x) (sub1 y))])))

(define o=
  (lambda (x y)
    (cond
      [(o> x y) #f]
      [(o< x y) #f]
      [else #t])))

(define pow
  (lambda (x y)
    (cond
      [(zero? y) 1]
      [else (* x (pow x (sub1 y)))])))

(define o/
  (lambda (x y)
    (cond
      [(< x y) 0]
      [else (add1 (o/ (- x y) y))])))

(define len
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (len (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond
      [(null? lat) (quote())]
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define lat (list 'lasaagna 'spaghetti 'ravioly 'macaroni 'cheese))

(define rempick
  (lambda (n lat)
    (cond
      [(null? lat) (quote())]
      [(zero? (sub1 n)) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(define non-nums
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [(number? (car lat)) (non-nums (cdr lat))]
      [else (cons (car lat) (non-nums (cdr lat)))])))

(define l (list 5 'pears 6 'prunes 9 'dates))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [(not (number? (car lat))) (all-nums (cdr lat))]
      [else (cons (car lat) (all-nums (cdr lat)))])))

(define eqan
  (lambda (x y)
    (cond
      [(and (number? x) (number? y)) (o= x y)]
      [(or (number? x) (number? y)) #f]
      [else (eq? x y)])))

(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eq? (car lat) a) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(define one?
  (lambda (x)
    (= x 1)))

(define rempick2
  (lambda (n lat)
    (cond
      [(null? lat) (quote())]
      [(one? n) (cdr lat)]
      [else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))])))