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

