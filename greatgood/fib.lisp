;; generate Fibonacci sequence
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) n)))