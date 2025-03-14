;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)4 of The Seasoned Schemer
(load "helpers.scm")

;; Extract the leftmost symbol
#;(define leftmost
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

;; Remove the leftmost ocurrence of a in l
(define rember1*
  (lambda (a l)
    (letrec
	((R (lambda (l)
	      (cond
	       ((null? l) '())
	       ((atom? (car l))
		(cond
		 ((eq? (car l) a) (cdr l))
		 (else (cons (car l)
			     (R (cdr l))))))
	       (else
		(cond
		 ((list?
		   (R (car l))
		   (car l))
		  (cons (car l)
			(R (cdr l))))
		 (else (cons (R (car l))
			     (cdr l)))))))))
      (R l))))


;; count how many lists of lists
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (let ((a (add1 (depth* (car l))))
	    (d (depth* (cdr l))))
	(if (> d a) d a))))))


;; Scramble again
(define scramble
  (lambda (tup)
    (letrec
	((P (lambda (tup rp)
	      (cond
	       ((null? tup) '())
	       (else
		(let ((rp (cons (car tup) rp)))
		  (cons (pick (car tup) rp)
			(P (cdr tup) rp))))))))
      (P tup '()))))


;; Revised leftmost
(define leftmost
  (lambda (l)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
	   ((lm (lambda (l)
		  (cond
		   ((null? l) '())
		   ((atom? (car l))
		    (skip (car l)))
		   (else
		    (begin
		      (lm (car l))
		      (lm (cdr l))))))))
	 (lm l))))))
