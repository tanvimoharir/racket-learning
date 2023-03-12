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
      [(eq? (car lat) old) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))])))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) (quote())]
      [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

(define mrember
  (lambda (a lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) a) (mrember a (cdr lat))]
      [else (cons (car lat) (mrember a (cdr lat)))])))

(define minsertR
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons old (cons new (minsertR new old (cdr lat))))]
      [else (cons (car lat) (minsertR new old (cdr lat)))])))

(define minsertL
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons new (cons old (minsertL new old (cdr lat))))]
      [else (cons (car lat) (minsertL new old (cdr lat)))])))

(define ls (list 'coffee 'cup 'tea 'cup 'and 'hick 'cup))

(define msubst
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons new (msubst new old (cdr lat)))]
      [else (cons (car lat) (msubst new old (cdr lat)))])))