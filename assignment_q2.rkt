#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!


;A
(provide ins_beg)

(define (ins_beg el lst)
        (cons el lst))

;B
(provide ins_end)

(define (ins_end el lst)
  (append lst (list el)))

;C
(provide count_top_level)

(define (count_top_level lst)
  (if
   (null? lst)
   0
   (+ 1 (count_top_level (cdr lst)))))

;D
(provide count_instances)

(define (count_instances lst item)
    (cond (
           (empty? lst)
           0)
          ((= (car lst) item)
           (+ 1 (count_instances (cdr lst) item)))
          (else
           (count_instances (cdr lst) item)))
  )

;E
(provide count_instances_tr)

(define (count_instances_tr lst item)
  (calculate_tr item lst 0))
(define (calculate_tr item lst total)
  (cond
    [(empty? lst) total]
    [(equal? item (car lst))
     (calculate_tr item (cdr lst) (+ 1 total))]
    [else (calculate_tr item
                        (cdr lst) total)]))



;A.
(display "(A) The ins_beg:\n")
(ins_beg 'a '(b c d))

;B.
(display "(B) ins_end:\n")
(ins_end '(a b) '(b c d))

;C.
(display "(C) count_top_level:\n")
(count_top_level '(1 2 3 (4 5 6)))

;D.
(display "(D) count_instances:\n")
(count_instances '(1 2 3 4 4) 4)

;E
(display "(E) count_instances_tr:\n")
(count_instances_tr '(1 2 3 4 4)4)





