;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)4 of The Seasoned Schemer
(load "helpers.scm")

;; Extract the leftmost symbol
(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (car l))
     (else
      (let ((a (leftmost (car l))))
	(cond
	 ((atom? a) a)
	 (else (leftmost (cdr l)))))))))

;; Remove the first member
(define rember1*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (cdr l))
       (else (cons (car l)
		   (rember1* a (cdr l))))))
     (else
      (cond
       ((list? (rember1* a (car l))
	       (car l))
	(cons (car l)
	      (rember1* a (cdr l))))
       (else (cons (rember1* a (car l))
		   (cdr l))))))))
