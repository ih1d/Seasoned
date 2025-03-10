;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)1 from The Seasoned Schemer
(load "helpers.scm")

;; Determines whether two atoms are in a row
;; in a list
#;(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat) (cdr lat))
	  (two-in-a-row? (cdr lat)))))))

;; Revised version
#;(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (is-first-b? (car lat) (cdr lat))))))

;; ReRevised version
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (two-in-a-row-b? (car lat) (cdr lat))))))

;; determines if the first element of a list
;; is equal to some element
(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? a (car lat))))))

;; Revised version
(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? a (car lat))
	  (two-in-a-row? (cdr lat)))))))

;; New version for is-first
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? preceding (car lat))
	  (two-in-a-row-b? (car lat)
			 (cdr lat)))))))

;; Sum of prefixes
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

;; sum of prefixes helper
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (+ sonssf (car tup))
	    (sum-of-prefixes-b
	     (+ sonssf (car tup))
	     (cdr tup)))))))

;; pick a number the nth number from a list
(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

;; helper for scramble
(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (pick (car tup)
		  (cons (car tup) rev-pre))
	    (scramble-b (cdr tup)
			(cons (car tup) rev-pre)))))))

;; Scramble definition
(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))
