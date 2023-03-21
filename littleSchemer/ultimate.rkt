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

(define rember-f2
  (lambda (test? a lat)
    (cond
      [(null? lat) (quote())]
      [(test? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember-f2 test? a (cdr lat)))])))

(lambda (a l) #t) ;procedure
(lambda (a)
  (lambda (x)
    (eq? x a)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) (quote())]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(define rember-eq?
  (rember-f eq?))

(define insertLo
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons new lat)]
      [else (cons (car lat) (insertLo new old (cdr lat)))])))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) (quote())]
        [(test? (car l) old)(cons new l)]
        [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

(define insertRo
  (lambda (new old lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertRo new old (cdr lat)))])))


(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) (quote())]
        [(test? (car l) old) (cons old (cons new (cdr l)))]
        [else (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) (quote())]
        [(eq? (car l) old) (seq new old (cdr l))]
        [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define ^
  (lambda (a x)
    (cond
      [(zero? (sub1 x)) a]
      [else (* a (^ a (sub1 x)))])))

(define atom-func
  (lambda (x)
    (cond
      [(eq? x (quote +)) +]
      [(eq? x (quote *)) *]
      [else ^])))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-func nexp) (value ((car nexp))
                                         (value ((car (cdr nexp))))))])))


(define mrember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) a) ((mrember-f test?) a (cdr lat))]
        [else (cons (car lat) ((mrember-f test?) a (cdr lat)))]))))

(define mrember-eq?
  (mrember-f eq?))

(define mremberT
  (lambda (test? lat)
    (cond
      [(null? lat) (quote())]
      [(test? (car lat)) (mremberT test? (cdr lat))]
      [else (cons (car lat) (mremberT test? (cdr lat)))])))

(define lat (list 'shrimp 'salad 'tuna 'salad 'and 'tuna))
(define eq?-tuna (eq?-c (quote tuna)))

(define mrember&co
  (lambda (a lat col)
    (cond
      [(null? lat) (col (quote()) (quote()))]
      [(eq? (car lat) a) (mrember&co a (cdr lat)
                                     (lambda (newlat seen)
                                       (col newlat
                                            (cons (car lat) seen))))]
      [else (mrember&co a (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons (quote tuna) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote and) newlat) seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define minsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) oldL)
       (cons new (cons oldL (minsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR (cons new (minsertLR new oldL oldR (cdr lat))))]
      [else
       (cons (car lat) (minsertLR new oldL oldR (cdr lat)))])))

(define minsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat)
       (col (quote()) 0 0)]
      [(eq? (car lat) oldL)
       (minsertLR&co new oldL oldR (cdr lat)
                     (lambda (newlat L R)
                       (col (cons new
                                  (cons oldL newlat))
                            (add1 L) R)))]
      [(eq? (car lat) oldR)
       (minsertLR&co new oldL oldR (cdr lat)
                     (lambda (newlat L R)
                       (col (cons oldR
                                  (cons new newlat))
                            L (add1 R))))]
      [else
       (minsertLR&co new oldL oldR (cdr lat)
                     (lambda (newlat L R)
                       (col (cons (car lat) newlat)
                            L R)))])))
(define even?
  (lambda (x)
    (= 0 (remainder x 2))))

(define evens-only*
  (lambda (l)
    (cond
      [(null? l) (quote())]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (cons (car l)
                (evens-only* (cdr l)))]
         [else (evens-only* (cdr l))])]
       [else (cons (evens-only* (car l)) (evens-only* (cdr l)))])))