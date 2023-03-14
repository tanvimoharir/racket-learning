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

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(equal? (car lat) a) #t]
      [else (member? a (cdr lat))])))

(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(define lat (list 'apple 'peaches 'apple 'plum))

(define mrember
  (lambda (a lat)
    (cond
      [(null? lat) (quote())]
      [(eq? (car lat) a) (mrember a (cdr lat))]
      [else (cons (car lat) (mrember a (cdr lat)))])))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) (quote())]
      [else (cons (car lat) (makeset (mrember (car lat) (cdr lat))))])
    ))


(define subset?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(member? (car s1) s2) (subset? (cdr s1) s2)]
      [else #f])))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(define intersect?
  (lambda (s1 s2)
    (cond
      [(null? s1) #f]
      [else (or(member? (car s1) s2) (intersect? (cdr s1) s2))])))

(define intersect
  (lambda (s1 s2)
    (cond
      [(null? s1) (quote())]
      [(member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2))]
      [else (intersect (cdr s1) s2)])))

(define union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member? (car s1) s2) (union (cdr s1) s2)]
      [else (cons (car s1) (union (cdr s1) s2))])))

(define s (list 'stewed 'tomatoes 'and 'macaroni))
(define s3 (list 'macaroni 'and 'cheese))

(define set1 (list 5 'chicken 'wings))
(define set2 (list 5 'hamburgers 2 'pieces
                     'fried 'chicken 'and
                     'light 'duckling 'wings))

(define t1 (list 4 'pounds 'of 'horseradish))
(define t2 (list 'four 'pounds 'chicken 'and 5 'ounces 'horseradish))

(define intersect-all
  (lambda (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set) (intersect-all (cdr l-set)))])))

(define l-set (list (list 'a 'b 'c) (list 'c 'a 'd 'e)
                    (list 'e 'f 'g 'h 'a 'b)))

(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define y1 (list 'apples 'peaces 'pumpkin 'pie))
(define y2 (list (list 'apples 'peaches)
                 (list 'pumpkin 'pie)
                 (list 'apples 'peaches)))
(define y3 (list (list 'apples 'peaches)
                 (list 'pumpkin 'pie)))

(define relation?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(and (and (pair? (first l)) (relation? (cdr l)))) (set? l)]
      [else #f])))

(define rel1 (list (list 8 3)
                  (list 4 2)
                  (list 7 6)
                  (list 6 2)
                  (list 3 4)))

(define rel2 (list (list 4 3)
                  (list 4 2)
                  (list 7 6)
                  (list 6 2)
                  (list 3 4)))

(define firsts
  (lambda (l)
    (cond
      [(null? l) (quote())]
      [else (cons (car (car l)) (firsts (cdr l)))])))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (cond
      [(null? pair) (quote())]
      [else (build (second pair) (first pair))])))

(define revrel
  (lambda (rel)
    (cond
      [(null? rel) (quote())]
      [else (cons (revpair (car rel))
                  (revrel (cdr rel)))])))

(define seconds
  (lambda (l)
    (cond
      [(null? l) (quote())]
      [else (cons (second (car l)) (seconds (cdr l)))])))

(define fullfun?
  (lambda (f)
    (set? (seconds f))))

(define r3 (list (list 8 3)
                   (list 4 8)
                   (list 7 6)
                   (list 6 2)
                   (list 3 4)))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
