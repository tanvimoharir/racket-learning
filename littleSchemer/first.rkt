#lang br

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define s-exp?
  (lambda (x)
    (or (atom? x) (list? x))))

