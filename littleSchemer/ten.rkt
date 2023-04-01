#lang racket

(define first
  (lambda (lat)
    (car lat)))

(define second
  (lambda (lat)
    (car (cdr lat))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      [(null? names) (entry-f name)]
      [(eq? (car names) name) (car values)]
      [else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)])))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define extend-table cons)
(define table '(((entree dessert)
                 (spaghetti spumoni))
                ((appetizer entree beverage)
                 (food tastes good))))

(define table-f
  (lambda (name)#t))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      [(null? table) (table-f name)]
      [else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))])))

(define rep-a 'a)
(define rep-b 'b)
(define rep-c 'c)

(cons rep-a
      (cons rep-b
            (cons rep-c
                  (quote()))))

(define rep-car 'car)
(define rep-quote 'quote)

(define e
  (cons rep-car
      (cons (cons rep-quote
                  (cons
                   (cons rep-a
                         (cons rep-b
                               (cons rep-c
                                     (quote()))))
                   (quote())))
            (quote()))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))


(define pow
  (lambda (x y)
    (cond
      [(zero? y) 1]
      [else (* x (pow x (sub1 y)))])))

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


(define atom-to-action
  (lambda (e)
    (cond
      [(number? e) *const]
      [(eq? e #t) *const]
      [(eq? e #f) *const]
      [(eq? e (quote cons)) *const]
      [(eq? e (quote car)) *const]
      [(eq? e (quote cdr)) *const]
      [(eq? e (quote null?)) *const]
      [(eq? e (quote eq?)) *const]
      [(eq? e (quote atom?)) *const]
      [(eq? e (quote zero?)) *const]
      [(eq? e (quote add1)) *const]
      [(eq? e (quote sub1)) *const]
      [(eq? e (quote number?)) *const]
      [else *identifier])))

(define list-to-action
  (lambda (e)
    (cond
      [(atom? (car e))
       (cond
         [(eq? (car e) (quote quote)) *quote]
         [(eq? (car e) (quote lambda)) *lambda]
         [(eq? (car e) (quote cond)) *cond]
         [else *application])]
      [else *application])))

(define value
  (lambda (e)
    (meaning e (quote()))))

(define meaning
  (lambda (e table)
    ((expr-to-action e) e table)))


(define expr-to-action
  (lambda (e)
    (cond
      [(atom? e) (atom-to-action e)]
      [else (list-to-action e)])))

(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build (quote primitive) e)])))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote()))))