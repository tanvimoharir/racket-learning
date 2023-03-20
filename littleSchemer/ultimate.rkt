#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(equal? (car l1) (car l2)) (and #t (eqlist? (cdr l1) (cdr l2)))]
      [else #f])))


(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eq? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(define rember-f
  (lambda (test? a lat)
    (cond
      [(null? lat) (quote())]
      [(test? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember-f test? a (cdr lat)))])))