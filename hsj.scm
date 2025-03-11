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
      (cond
       ((null? set2) '())
       (else (I set1))))))

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
#;(define intersectall
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
		  (else (I (car lset)
			   (A (cdr lset)))))))
	    (I (lambda (s1 s2)
		 (letrec
		     ((J (lambda (s1)
			   (cond
			    ((null? s1) (hop '()))
			    ((member? (car s1) s2)
			     (J (cdr s1)))
			    (else (cons (car s1)
					(J (cdr s1))))))))
		   (cond
		    ((null? s2) '())
		    (else (J s1))))))
	 (cond
	  ((null? lset) '())
	  (else (A lset)))))))))

;; Remove member using letrec
(define rember
  (lambda (a lat)
    (letrec
	((R (lambda (l)
	      (cond
	       ((null? l) '())
	       ((eq? a (car l))
		(cdr l))
	       (else (cons (car l)
			   (R (cdr l))))))))
      (R lat))))

;; Remove beyond first
(define rember-beyond-first
  (lambda (a lat)
    (letrec
	((R (lambda (lat)
	      (cond
	       ((null? lat) '())
	       ((eq? (car lat) a)
		'())
	       (else (cons (car lat)
			   (R (cdr lat))))))))
      (R lat))))

;; Remove up to last
(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
	   ((R (lambda (lat)
		 (cond
		  ((null? lat) '())
		  ((eq? (car lat) a)
		   (skip (R (cdr lat))))
		  (else
		   (cons (car lat)
			 (R (cdr lat))))))))
	 (R lat))))))
