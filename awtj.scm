;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)9 from The Reasoned Schemer
(load "helpers.scm")

(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else
      (cons (deep (sub1 m))
	    '())))))

(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
	(cons
	 (cons p '())
	 '())
	'())
       '())
      '())
     '())))

(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())))

(define toppings)

(define deepB
  (lambda (m)
    (cond
     ((zero? m)
      (call-with-current-continuation
       (lambda (jump)
	 (set! toppings jump)
	 'pizza)))
     (else (cons (deepB (sub1 m))
		 '())))))

(define deep&co
  (lambda (m k)
    (cond
     ((zero? m) (k 'pizza))
     (else
      (deep&co (sub1 m)
	       (lambda (x)
		 (k (cons x '()))))))))

(define two-layers
  (lambda (p)
    (cons
     (cons p '())
     '())))

(define deep&coB
  (lambda (m k)
    (cond
     ((zero? m)
      (let ()
	(set! toppings k)
	(k 'pizza)))
     (else
      (deep&coB (sub1 m)
		(lambda (x)
		  (k (cons x '()))))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-b? (car lat)
			    (cdr lat))))))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (two-in-a-row-b? (car lat)
				(cdr lat)))))))
(define leave)

(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (leave (car l)))
     (else
      (let ()
	(walk (car l))
	(walk (cdr l)))))))

(define start-it
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (walk l)))))

(define fill)
(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (let ()
	(call-with-current-continuation
	 (lambda (rest)
	   (set! fill rest)
	   (leave (car l))))
	(waddle (cdr l))))
     (else
      (let ()
	(waddle (car l))
	(waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (waddle l)))))

(define get-next
  (lambda (x)
    (call-with-current-continuation
     (lambda (here-again)
       (set! leave here-again)
       (fill 'go)))))

(define get-first
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (waddle l)
       (leave '())))))

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
	     (let ((n (get-next 0)))
	       (if (atom? n)
		   (or (eq? n a)
		       (T? n))
		   #f))))
       (get-next
	(lambda (x)
	  (call-with-current-continuation
	   (lambda (here-again)
	     (set! leave here-again)
	     (fill 'go)))))
       (fill (lambda (x) x))
       (waddle
	(lambda (l)
	  (cond
	   ((null? l) '())
	   ((atom? (car l))
	    (let ()
	      (call-with-current-continuation
	       (lambda (rest)
		 (set! fill rest)
		 (leave (car l))))
	      (waddle (cdr l))))
	   (else (let ()
		   (waddle (car l))
		   (waddle (cdr l))))))))
    (leave (lambda (x) x)))
  (lambda (l)
    (let ((fst (call-with-current-continuation
		(lambda (here)
		  (set! leave here)
		  (waddle l)
		  (leave '())))))
      (if (atom? fst)
	  (T? fst)
	  #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
	  (or (eq? n a)
	      (two-in-a-row-b*? n))
	  #f))))

