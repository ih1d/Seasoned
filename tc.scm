;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)2 The Seasoned Schemer

;; Remove multiple times a member from a list
#;(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
	  (lambda (lat)
	    (cond
	     ((null? lat) '())
	     ((eq? a (car lat))
	      (mr (cdr lat)))
	     (else (cons (car lat)
			 (mr (cdr lat))))))))
     lat)))

;; better way to remove multiple times a member from a list
;; using letrec
(define multirember
  (lambda (a lat)
    ((letrec
	 ((mr (lambda (lat)
		(cond
		 ((null? lat) '())
		 ((eq? a (car lat))
		  (mr (cdr lat)))
		 (else
		  (cons (car lat)
			(mr (cdr lat))))))))
       mr)
     lat)))

;; remove the first member given a test
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
		   ((rember-f test?) a
		    (cdr l))))))))

;; remove multiple members of a given test
#;(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
	((multirember-f test?) a
	 (cdr lat)))
       (else
	(cons (car lat)
	      ((multrember-f test?) a
	       (cdr lat))))))))

;; revised version
(define multirember-f
  (lambda (test?)
    (letrec
	((m-f
	  (lambda (a lat)
	    (cond
	     ((null? lat) '())
	     ((test? (car lat) a)
	      (m-f a (cdr lat)))
	     (else
	      (cons (car lat)
		    (m-f a (cdr lat))))))))
      m-f)))

;; membership of element in list
(define member?
  (lambda (a lat)
    ((letrec
	 ((yes? (lambda (l)
		  (cond
		   ((null? l) #f)
		   ((eq? a (car l)) #t)
		   (else (yes? (cdr l)))))))
       yes?)
     lat)))

;; union of two sets
#;(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))

;; revised version
#;(define union
  (lambda (set1 set2)
    (letrec
	((U (lambda (set)
	      (cond
	       ((null? set) set2)
	       ((member? (car set) set2)
		(U (cdr set)))
	       (else
		(cons (car set)
		      (U (cdr set))))))))
      (U set1))))

;; rerevised version
(define union
  (lambda (set1 set2)
    (letrec
	((M? (lambda (lat a)
		    (cond
		     ((null? lat) #f)
		     ((eq? (car lat) a) #t)
		     (else
		      (M? (cdr lat) a)))))
	 (U (lambda (set)
	      (cond
	       ((null? set) set2)
	       ((M? set2 (car set))
		(U (cdr set)))
	       (else
		(cons (car set)
		      (U (cdr set))))))))
      (U set1))))
