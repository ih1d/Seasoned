;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Chapter (1)3 of The Seasoned Schemer
(load "helpers.scm")

;; Intersection of two sets
(define intersect
  (lambda (set1 set2)
    (letrec
	((I (lambda (set)
	      (cond
	       ((null? set) '())
	       ((member? (car set) set2)
		(cons (car set)
		      (I (cdr set))))
	       (else
		(I (cdr set)))))))
      (I set1))))

;; Intersect a list of sets
#;(define intersectall
  (lambda (lset)
    (letrec
	((A (lambda (lset)
	      (cond
	       ((null? (cdr lset))
		(car lset))
	       (else (intersect (car lset)
				(A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A lset))))))


;; Revised version
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
	   ((A (lambda (lset)
		 (cond
		  ((null? (car lset))
		   (hop '()))
		  ((null? (cdr lset))
		   (car lset))
		  (else
		   (intersect (car lset)
			      (A (cdr lset))))))))
	 (cond
	  ((null? lset) '())
	  (else (A lset))))))))
