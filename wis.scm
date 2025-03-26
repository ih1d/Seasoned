;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: The Seasoned Schemer chapter 10/20
(load "helpers.scm")

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
       ((eq? name2 name1) value)
       (else (table name2))))))

(define x 3)

(define value
  (lambda (e)
    ...
    (cond
     ((define? e) (*define e))
     (else (the-meaning e))) ...))

(define define?
  (lambda (e)
    (cond
     ((atom? e) #f)
     ((atom? (car e))
      (eq? (car e) 'define))
     (else #f))))

(define global-table
  ... the-empty-table ...)

(define *define
  (lambda (e)
    (set! global-table
	  (extend
	   (name-of e)
	   (box
	    (the-meaning
	     (right-side-of e)))
	   global-table))))

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
		(set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define meaning
  (lambda (e table)
    ((expression-to-action e)
     e table)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
	      (multi-extend
	       (formals-of e)
	       (box-all args)
	       table)))))

(define beglis
  (lambda (es table)
    (cond
     ((null? (cdr es))
      (meaning (car es) table))
     (else ((lambda (val)
	      (beglis (cdr es) table))
	    (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond
     ((null? vals) '())
     (else (cons (box (car vals))
		 (box-all (cdr vals)))))))

(define multi-extend
  (lambda (names values table)
    (cond
     ((null? names) table)
     (else
      (extend (car names) (car values)
	      (multi-extend
	       (cdr-names)
	       (cdr-values)
	       table))))))

(define odd?
  (lambda (n)
    (cond
     ((zero? n) #t)
     (else (even? (sub1 n))))))

(define even?
  (lambda (n)
    (cond
     ((zero? n) #t)
     (else (odd? (sub1 n))))))

(define *aplication
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      ((lambda (val)
	 (cons val
	       (evlis (cdr args) table)))
       (meaning (car args) table))))))

(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list
	      (car (cdr args-in-a-list)))))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     ((eq? e 'cons)
      (b-prim cons))
     ((eq? e 'car)
      (a-prim car))
     ((eq? e 'eq?)
      (b-prim eq?))
     ((eq? e 'atom?)
      (a-prim atom?))
     ((eq? e 'null?)
      (a-prim null?))
     ((eq? e 'zero?)
      (a-prim zero?))
     ((eq? e 'add1)
      (a-prim add1))
     ((eq? e 'sub1)
      (a-prim sub1))
     ((eq? e 'number?)
      (a-prim number?)))))
