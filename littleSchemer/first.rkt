#lang br

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define s-exp?
  (lambda (x)
    (or (atom? x) (list? x))))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(define l (list (list 'apple 'peach 'pumpkin) (list 'plum 'pear 'cherry) (list 'grape 'raisin 'pea) (list 'bean 'carrot 'eggplant)))

(define firsts
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [else (cons (car (car lat)) (firsts (cdr lat)))])))

(define l2 (list 'ice 'cream 'with 'fudge 'for 'dessert))

(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons new (cons old (cdr lat)))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))