#lang typed/racket

(provide: 
 [zip (All (A B) (Listof A) (Listof B) -> (Listof (Pair A B)))]
 [random-number-list (Index Index -> (Listof Real))]
 [random-symbol-list (Index Index String (Listof Symbol) -> (Listof Symbol))])

(: zip (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (zip lsta lstb)
  (let: loop : (Listof (Pair A B)) ((as : (Listof A) lsta) 
                                    (bs : (Listof B) lstb) 
                                    (accum : (Listof (Pair A B))'()))
    (if (or (null? as)
            (null? bs))
        (reverse accum)
        (loop (cdr as) (cdr bs) (cons (cons (car as) (car bs)) accum)))))

(: random-number-list (Index Index -> (Listof Real)))
(define (random-number-list n max)
  (for/list ((i n))
    (add1 (random max))))

(: random-symbol-list (Index Index String (Listof Symbol) -> (Listof Symbol)))
(define (random-symbol-list n max rolling-string (ol '()))
  (let ((str : String (string-append rolling-string (string (integer->char (+ 97 (random max)))))))
    (if (= n (+ (length ol) 1))
        ol
        (random-symbol-list n max str (cons (string->symbol str) ol)))))
