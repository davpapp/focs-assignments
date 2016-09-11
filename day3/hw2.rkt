#lang racket

;;; Student Name: David Papp [change to your name]
;;;
;;; Check one:
;;; [x] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___


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
					(calculate (first (rest (rest (rest x))))))])))

;;; 1.  Create a calculator that takes one argument: a list that represents an expression.

(calculate '(ADD 3 4)) ;; --> 7

;;; 2. Expand the calculator's operation to allow for arguments that are themselves well-formed arithmetic expressions.

(calculate '(ADD 3 (MUL 4 5))) ;; --> 23   ;; what is the equivalent construction using list?
(calculate '(SUB (ADD 3 4) (MUL 5 6))) ;; --> -23

;;; 3. Add comparators returning booleans (*e.g.*, greater than, less than, …).
;; Note that each of these takes numeric arguments (or expressions that evaluate to produce numeric values),
;; but returns a boolean.  We suggest operators `GT`, `LT`, `GE`, `LE`, `EQ`, `NEQ`.

(calculate '(GT (ADD 3 4) (MUL 5 6))) ;; --> #f
(calculate '(LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6))))) ;; --> #t

;;; 4. Add boolean operations ANND, ORR, NOTT

(calculate '(ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))) ;; --> #

;;; 5. Add IPH

(calculate '(IPH (GT (ADD 3 4) 7) (ADD 1 2) (ADD 1 3))) ;; -> 4
