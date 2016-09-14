#lang racket

;; David Papp
;;classwork3.rkt

;; 0. Factorial

(define (factorial x)
	(if (= x 1)
		1
		(* x (factorial (- x 1)))))

(define (tail-factorial x)
	(tail-factorial-helper x 1))

(define (tail-factorial-helper x counter)
	(if (= x 1)
		counter
		(tail-factorial-helper (- x 1) (* counter x))))

(factorial 5)
(tail-factorial 5)

;; 1. Filter


(define (my-filter fltr lst)
	(my-filter-helper fltr lst empty))

;; Tail recursive
(define (my-filter-helper fltr lst newlst)
	(if (empty? lst)
		newlst
		(if (fltr (first lst))
			(my-filter-helper fltr (rest lst) (append newlst (list (first lst))))
			(my-filter-helper fltr (rest lst) newlst))))

(define (teen? x)
	(and (<= 13 x) (<= x 19)))


(my-filter even? '(1 2 3 4 5 6))
(my-filter teen? '(21 17 2 13 4 42 2 16 3))
(my-filter list? '(3 (3 2 1) symbol (4 2) (1 (2) 3)))

;; 2. Map -------------------------------------------------------------------------------

;; Tail
(define (my-map command lst)
	(my-map-helper command lst empty))

(define (my-map-helper command lst newlst)
	(if (empty? lst)
		newlst
		(my-map-helper command (rest lst) (append newlst (list (command (first lst)))))))

(define (double x)
	(* x 2))

(define (incr x)
	(+ x 1))

(define (last lst)
	(if (null? (rest lst))
		(first lst)
		(last (rest lst))))

(my-map double '(1 2 3))
(my-map incr '(1 2 3))
(my-map last '((3 2 1) (4 2) (1 2 3)))

;; 3. Append ----------------------------------------------------------------------------

;; I don't entirely understand the definition of "functional".
;; It seems to me that this function doesn't modify either list, just passes the rest of the first list in each recursive step. This function is not tail recursive.

(define (my-append lst1 lst2)
	(if (empty? lst1)
		lst2
		(cons (first lst1) (my-append (rest lst1) lst2))))
		

(my-append '(1 2 3) '(4 5 6))

;; 4. Zip --------------------------------------------------------------------------------

(define (zip lst1 lst2)
	(zip-helper lst1 lst2 empty))

(define (zip-helper lst1 lst2 newlst)
	(if (empty? lst1)
		newlst
		(if (empty? lst2)
			newlst
			(zip-helper (rest lst1) (rest lst2) (append newlst (list (list (first lst1) (first lst2))))))))

(zip '(1 2 (4 5) 3) '(4 5 6))
(zip '(1 2 3) '(a b c d e f g))


;; 5. Reverse ----------------------------------------------------------------------------

(define (my-reverse lst)
	(my-reverse-helper lst empty))

(define (my-reverse-helper lst newlst)
	(if (empty? lst)
		newlst
		(my-reverse-helper (rest lst) (append (list (first lst)) newlst))))

(my-reverse '(1 2 3))

;; I was going to implement my-reverse such that it also reverses nested lists,
;; but it seems that the built in reverse doesn't do that.
