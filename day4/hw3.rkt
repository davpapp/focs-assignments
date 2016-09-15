#lang racket

;;; Student Name: David Papp
;;;
;;; Check one:
;;; [x] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___


;;;;;;;;;;;
;; 1. assq

;; `assq` is a function that takes a key and an association list.
;;
;; It returns the corresponding key/value pair from the list
;; (*i.e.*, the pair whose key is *eq?* to the one it is given).
;;
;; If the key is not found in the list, `assq` returns `#f`.

(define (assq key lst)
	(if (empty? lst)
		#f
		(if (eq? key (first (first lst)))
			(cons key (second (first lst)))
			(assq key (rest lst)))))

(assq 'c '((a aaa) (b bbb) (c ccc))) ;; --> '(c ccc)



;;;;;;;;;;;
;; 2. lookup-list

;; Add the ability to look up symbols to your evaluator.
;;
;; Add the `lookup-list` argument to your hw2 evaluator (or ours, from the solution set).
;; `(evaluate 'foo lookup-list)` should return whatever `'foo` is associated with in `lookup-list`.


(define (calculate x)
	(if (number? x)
		x
		(cond 
			[(equal? (first x) 'ADD)
				(+ (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'SUB)
				(- (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'MUL)
				(* (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'DIV)
				(/ (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'GT)
				 (> (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'GT)
				 (> (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'LT)
				 (< (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'GE)
				 (>= (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'LE)
				 (<= (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'EQ)
				 (= (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'NEQ)
				 (not (= (calculate (first (rest x))) (calculate (first (rest (rest x))))))]
			[(equal? (first x) 'ANND)
				 (and (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'ORR)
				 (or (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'NOTT)
				 (not (calculate (first (rest x))) (calculate (first (rest (rest x)))))]
			[(equal? (first x) 'IPH)
				(if (calculate (first (rest x)))
					(calculate (first (rest (rest x))))
					(calculate (first (rest (rest (rest x))))))]
			[(equal? (first x) 'lookup-list)
				(lookup (second x) (third x))])))

(define (lookup key lst)
	(if (empty? lst)
		#f
		(if (eq? key (first (first lst)))
			(second (first lst))
			(assq key (rest lst)))))

(calculate '(lookup-list a ((a aaa) (b bbb) (c ccc))))