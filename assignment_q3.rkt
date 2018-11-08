#lang racket

(define (left_tree tree)
         (car tree))

(define (right_tree tree)
         (car tree))

(define (value_tree tree)
         (car tree))



;A
(define (sort_tree tree)
 (begin(cond [(not (empty?(left_tree tree))) (sort_tree (left_tree tree))])
   (printf "~a " (value_tree tree))
   (cond [(not (empty?(right_tree tree))) (sort_tree (right_tree tree))]))
  )
  

;B
(define (search el tree)
(cond
  [(null? tree) #f]
  [(equal? tree (left_tree tree) )#t]
  [else (search el (right_tree tree) )])
  
  )

;C
(define (insert el tree)
  (cond ((empty? tree) (list '() el '()))
        ((equal? el (cadr tree)) tree)
        ((< el (cadr tree))
         (list (insert el(left_tree tree)) (cadr tree) (right_tree tree)))
        (else (list (left_tree tree) (cadr tree) (insert el (right_tree tree)))))
  )

;D
(define (insert_list el tree)
 (if(empty? el) tree
    (insert_list (cdr el) (insert (car el) tree)))
  )

(define bst '(((() 2 ()) 14 (() 6 ())) 5 ((() 4 ()) 12 (() 10 ()))))

;Part A
(display "(A) sorted:\n")
(sort_tree bst)

(display "\n")

 ;Part B
(display "(B) Searching:\n")
(search 7 bst) ;#f
(search 2 bst) ;#t

 ;Part C
(display "(C) Insert Element (1):\n")
(insert 1 bst)

 ;Part D
(display "(D) Insert list (4 9):\n")
(insert_list '(4 9 14)bst)
