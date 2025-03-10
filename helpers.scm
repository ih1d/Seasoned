;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Functions from The Little Schemer

;; Test to see if value is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))

;; Increase by 1 the current number
(define add1
  (lambda (x)
    (+ x 1)))

;; Decrease by 1 the current number
(define sub1
  (lambda (x)
    (- x 1)))

;; Test whether an atom is on a list
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))

;; Test whether a number is equal to one
(define one?
  (lambda (x)
    (= x 1)))
