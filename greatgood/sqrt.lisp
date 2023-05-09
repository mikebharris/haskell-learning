;; redefine operators to be floating point
(constant '+ add '- sub '* mul '/ div)

(define (square x)
    (* x x))

(define (average x y)
    (/ (+ x y) 2))

(define (absolute x)
    (if (< x 0)
	(- x)
	x))

;; myroot uses a block structure to encapsulate improve, try, and good-enough?
;; within it
(define (myroot x)
    (define (improve g)
        (average g (/ x g)))
    (define (good-enough? g)
        (< (absolute (- (square g) x))
            .001))
    (define (try g)
        (if (good-enough? g)
            g
            (try (improve g) x)))
    (try 1))