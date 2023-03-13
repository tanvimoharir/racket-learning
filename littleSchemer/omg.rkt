#lang racket

(define rember*
  (lambda (a l)
    (cond
      [(null? l) (quote())]
      [(list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))]
      [(eq? (car l) a) (rember* a (cdr l))]
      [else (cons (car l) (rember* a (cdr l)))])))

(define l (list (list 'coffee) 'cup
                (list (list 'tea) 'cup)
                (list 'and (list 'hick)) 'cup))

(define lat (list (list (list 'tomato 'sauce))
                  (list (list 'bean) 'sauce)
                  (list 'and (list (list 'flying)) 'sauce)))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) (quote())]
      [(list? (car l)) (cons (insertR* new old (car l))
                             (insertR* new old (cdr l)))]
      [(eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
      [else (cons (car l) (insertR* new old (cdr l)))])))

(define b (list
           (list 'banana)
           (list 'split
                 (list (list (list (list 'banana 'ice)))
                       (list 'cream (list 'banana)) 'sherbet))
           (list 'banana)
           (list 'bread)
           (list 'banana 'brandy)))

(define occur*
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(list? (car lat)) (+ (occur* a (car lat)) (occur* a (cdr lat)))]
      [(eq? (car lat) a) (add1 (occur* a (cdr lat)))]
      [else (occur* a (cdr lat))])))

(define subst*
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(list? (car lat)) (cons (subst* new old (car lat))
                               (subst* new old (cdr lat)))]
      [(eq? (car lat) old) (cons new (subst* new old (cdr lat)))]
      [else (cons (car lat) (subst* new old (cdr lat)))])))

(define insertL*
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(list? (car lat)) (cons (insertL* new old (car lat))
                               (insertL* new old (cdr lat)))]
      [(eq? (car lat) old) (cons new (cons old(insertR* new old (cdr lat))))]
      [else (cons (car lat) (insertL* new old (cdr lat)))])))

(define member*
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(list? (car lat)) (or (member* a (car lat)) (member* a (cdr lat)))]
      [(eq? (car lat) a) #t]
      [else (member* a (cdr lat))])))

(define leftmost
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [(list? (car lat)) (leftmost (car lat))]
      [else (car lat)])))

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

(define l2 (list 'tofu (list (list 'nuts)) (list 'and (list 'soda))))
(define l3 (list 'tofu (list (list 'nuts)) (list 'and (list 'soda))))

(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eq? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))