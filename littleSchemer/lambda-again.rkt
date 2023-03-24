#lang racket

(define rember-f2
  (lambda (test? a lat)
    (cond
      [(null? lat) (quote())]
      [(test? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember-f2 test? a (cdr lat)))])))

(lambda (a l) #f)

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
    (lambda (a lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) a) (cdr lat)]
        [else (cons (car lat) ((rember-f test?) a (cdr lat)))]))))

(define rember-eq?
  (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) old) (cons new  (cons old (cdr lat)))]
        [else (cons (car lat)
                    ((insertL-f test?) new old (cdr lat)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) old) (cons old (cons new (cdr lat)))]
        [else (cons (car lat)
                    ((insertR-f test?) new old (cdr lat)))]))))


(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))

(define insert-g
  (lambda (test?)
    (lambda (seq new old lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) old) (seq new old (cdr lat))]
        [else (cons (car lat)
                    ((insert-g test?) new old (cdr lat)))]))))

(define insertL
  (insert-g seqL))

(define insertR
  (insert-g
   (lambda (new old lat)
     (cons old (cons new lat)))))

(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst
  (insert-g seqS))

(define operator
  (lambda (op)
    (car op)))

(define first-sub
  (lambda (exp)
    (car (cdr exp))))

(define second-sub
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define ^
  (lambda (a n)
    (cond
      [(zero? n) 1]
      [else (* a (^ a (sub1 n)))])))

(define atom-func
  (lambda (x)
    (cond
      [(eq? x (quote +)) +]
      [(eq? x (quote *)) *]
      [else ^ ])))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (empty? x)))))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-func (operator nexp))
             (value (first-sub nexp)) (value (second-sub nexp)))])))

(define mrember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) (quote())]
        [(test? (car lat) a) ((mrember-f test?) a (cdr lat))]
        [else (cons (car lat) ((mrember-f test?) a (cdr lat)))]))))

(define mrember-eq?
  (mrember-f eq?))

;combining a and test since that doesnt change
;as func goes through lat

(define eq?-tofu
  (eq?-c 'tofu))

;redefining mrember which takes eq?-tuna like func as arg

(define mremberT
  (lambda (test? lat)
    (cond
      [(null? lat) (quote())]
      [(test? (car lat)) (mremberT test? (cdr lat))]
      [else (cons (car lat) (mremberT test? (cdr lat)))])))

(define lat (list 'tofu 'peas 'and 'ever 'green 'and 'salad 'tofu))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define mrember&co
  (lambda (a lat col)
    (cond
      [(null? lat) (col (quote()) (quote()))]
      [(eq? (car lat) a)
       (mrember&co a (cdr lat)
                   (lambda (newlat seen)
                     (col newlat (cons (car lat) seen))))]
      [else (mrember&co a (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat) seen)))])))

;combining multiinsert left and right
(define minsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) oldL)
       (cons new (cons oldL (minsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR (cons new (minsertLR new oldL oldR (cdr lat))))]
      [else (cons (car lat) (minsertLR new oldL oldR (cdr lat)))])))

; writing minsert with collector

(define minsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat) (col (quote()) 0 0)]
      [(eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (minsertLR&co new oldL oldR (cdr lat)
                                 (lambda (newlat L R)
                                   (col (cons new (cons oldL newlat))
                                        (add1 L) R)))))]
      [(eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (minsertLR&co new oldL oldR (cdr lat)
                                 (lambda (newlat L R)
                                   (col (cons oldR (cons new newlat))
                                        L (add1 R))))))]
      [else
       (cons (car lat)
             (minsertLR&co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons (car lat) newlat) L R))))])))

(define last-friend2
  (lambda (x y z)
    (length x)))

(define even?
  (lambda (n)
    (eq? 0 (remainder n 2))))

(define evens-only*
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [(atom? (car lat))
       (cond
         [(even? (car lat)) (cons (car lat) (evens-only* (cdr lat)))]
         [else (evens-only* (cdr lat))])]
      [else (cons (evens-only* (car lat)) (evens-only* (cdr lat)))])))

;evens only with collector

(define evens-only*&co
  (lambda (lat col)
    (cond
      [(null? lat) (col '() 0 1)]
      [(atom? (car lat))
       (cond
         [(even? (car lat))
          (evens-only*&co (cdr lat)
                          (lambda (newlat sum pro)
                            (col (cons (car lat) newlat)
                                       sum
                                       (* (car lat)pro))))]
         [else (evens-only*&co (cdr lat)
                               (lambda (newlat sum pro)
                                 (col newlat (+ (car lat) sum)
                                      pro)))])]
      [else
        (evens-only*&co (car lat)
                         (lambda (al as ap)
                          (evens-only*&co (cdr lat)
                                          (lambda (dl ds dp)
                                            (col (cons al dl)
                                                 (+ as ds)
                                                 (* ap dp))))))])))

(define the-last-friend
  (lambda (newlat sum product)
    (cons product
          (cons sum newlat))))

(define l '((9 1 2 8) 3 10 ((9 9) 7 6) 2))