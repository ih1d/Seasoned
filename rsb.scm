;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter (1)6 of The Seasoned Schemer
(load "helpers.scm")

(define sweet-tooth
  (lambda (food)
    (cons food
	  (cons 'cake '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
	  (cons 'cake '()))))

(define ingridients '())

(define sweet-toothR
  (lambda (food)
    (set! ingridients (cons food ingridients))
    (cons food
	  (cons 'cake '()))))

(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deepM (sub1 m))
		 '())))))

(define Ns '())

(define Rs '())

(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(define find
  (lambda (n Ns Rs)
    (letrec
	((A (lambda (ns rs)
	      (cond
	       ((null? ns) #f)
	       ((= (car ns) n) (car rs))
	       (else
		(A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM
  (let ((Rs '())
	(Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
	(if (atom? exists)
	    (let ((result (deep n)))
	      (set! Rs (cons result Rs))
	      (set! Ns (cons n Ns))
	      result)
	    exists)))))

(define Y-bang
  (lambda (f)
    (letrec
	((h (f (lambda (arg) (h arg)))))
      h)))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h
	    (L (lambda (arg) (h arg))))
      h)))

(define L
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define length (Y! L))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond
       ((null? s) 1)
       ((atom? (car s))
	(depth* (cdr s)))
       (else
	(max
	 (add1 (depth* (car s)))
	 (depth* (cdr s))))))))

(define depth* (Y! D))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
	(if (= a x)
	    0
	    (f a))))))
