;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)7 from The Seasoned Schemer
(load "helpers.scm")

(define deep
  (lambda (m)
    (if (zero? m)
	'pizza
	(consC (deep (sub1 m))
	      '()))))

(define deepM
  (let ((Rs '())
	(Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
	(if (atom? exists)
	    (let ((result
		   (let ((m n))
		     (if (zero? m)
			 'pizza
			 (cons (deepM (sub1 m))
			       '())))))
	      (set! Rs (cons result Rs))
	      (set! Ns (cons n Ns))
	      result)
	    exists)))))

(define counter)
(define consC
  (let ((N 0))
    (set! counter
	  (lambda ()
	    N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
