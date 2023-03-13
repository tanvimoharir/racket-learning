#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define numbered?
  (lambda (aexpr)
    (cond
      [(and (atom? aexpr) (number? aexpr)) #t]
      [(eq? (car (cdr aexpr)) (quote +)) #t]
      [(eq? (car (cdr aexpr)) (quote x)) #t]
      [(eq? (car (cdr aexpr)) (quote ^)) #t]
      [(and (numbered? (car aexpr)) (numbered? (car (cdr (cdr aexpr)))))]
      [else #f])))

(define pow
  (lambda (x y)
    (cond
      [(zero? y) 1]
      [else (* x (pow x (sub1 y)))])))

(define val
  (lambda (aexp)
    (cond
      [(and (atom? aexp) (number? aexp)) aexp]
      [(eq? (car (cdr aexp)) (quote +)) (+ (val (car aexp))
                                            (val (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) (quote x)) (* (val (car aexp))
                                            (val (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) (quote ^)) (pow (val (car aexp))
                                            (val (car (cdr (cdr aexp)))))]
      )))

(define first-sub
  (lambda (exp)
    (car (cdr exp))))

(define second-sub
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
  (lambda (op)
    (car op)))

(define val2
  (lambda (aexp)
    (cond
      [(and (atom? aexp) (number? aexp)) aexp]
      [(eq? (operator aexp) (quote +)) (+ (val2 (first-sub aexp))
                                            (val2 (second-sub aexp)))]
      [(eq? (operator aexp) (quote x)) (* (val2 (first-sub aexp))
                                            (val2 (second-sub aexp)))]
      [(eq? (operator aexp) (quote ^)) (pow (val2 (first-sub aexp))
                                            (val2 (second-sub aexp)))]
      )))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define t+
  (lambda (m n)
    (cond
      [(sero? n) m]
      [else (edd1 (t+ m (zub1 n)))])))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))