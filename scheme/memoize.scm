(define (fib n)
  (case n
    [(0 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (memoize proc)
  (let ([cache '()])
    (lambda (x)
      (cond [(assq x cache) => cdr]
            [else (let ([ans (proc x)])
                    (set! cache (cons (cons x ans) cache))
                    ans)]))))

(define fib/memo
  (memoize (lambda (n)
             (case n
               [(0 1) 1]
               [else (+ (fib/memo (- n 1)) (fib/memo (- n 2)))]))))