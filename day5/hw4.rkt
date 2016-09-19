#lang racket

;;; Student Name: David Papp
;;;
;;; Check one:
;;; [] I completed this assignment without assistance or external resources.
;;; [x] I completed this assignment with assistance from NINJA Ann
;;;     and/or using these external resources: ___

;; I was very confused by the instructions for part 3 and 4. Did you want part 3 to return a string representation? did you want part 4 to return a procedure or an actual evaluation? I tried to get the evaluation but I could only get part 4 to return a procedure. To get this procedure, you have to call APPLY. Again, I was confused by how you wanted this implemented here...
;;After 3+ hours of work, this is how far I got.


(define (run-repl)
  (display "welcome to my repl.  type some scheme-ish")
  (repl '((x 3) (y 2))))

(define (repl env)
  (display "> ")
  (display (calculate (read) env))
  (newline)
  (repl env))


(define (calculate x env)
	(if (number? x)
		x
		(if (equal? (lookup x env) #f)
			(cond 
				[(equal? (first x) 'ADD)
					(+ (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'SUB)
					(- (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'MUL)
					(* (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'DIV)
					(/ (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'GT)
					 (> (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'GT)
					 (> (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'LT)
					 (< (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'GE)
					 (>= (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'LE)
					 (<= (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'EQ)
					 (= (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'NEQ)
					 (not (= (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env)))]
				[(equal? (first x) 'ANND)
					 (and (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'ORR)
					 (or (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'NOTT)
					 (not (calculate (first (rest x)) env) (calculate (first (rest (rest x))) env))]
				[(equal? (first x) 'IPH)
					(if (calculate (first (rest x)) env)
						(calculate (first (rest (rest x))) env)
						(calculate (first (rest (rest (rest x)))) env))]
				[(equal? (first x) 'lookup-list)
					(lookup (second x) (third x))]
				[(equal? (first x) 'DEFINE)
					(repl (append env (list (list (second x) (calculate (third x) env)))))]
				[(equal? (first x) 'LAMBDA)
					(list 'lambda (second x) (third x) env)]
				[(equal? (first x) 'APPLY)
					(apply-proc (second x))])
			(lookup x env))))

(define (apply-proc x)
	(lambda (second (first x)) (third (first x)) (rest x)))

;;((lambda (x y) (+ x y)) 3 5)
;;(apply-proc '((LAMBDA (x y) (+ x y)) 3 5))

(define (lookup key lst)
	(if (empty? lst)
		#f
		(if (eq? key (first (first lst)))
			(second (first lst))
			(lookup key (rest lst)))))

(define (assq key lst)
	(if (empty? lst)
		#f
		(if (eq? key (first (first lst)))
			(cons key (second (first lst)))
			(assq key (rest lst)))))

(run-repl)