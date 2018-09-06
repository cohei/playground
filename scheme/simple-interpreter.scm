;plus interpreter
;(let loop ([n (read)]
;           [sum 0])
;  (if (eq? n 'end)
;      sum
;      (loop (read) (+ n sum))))

;scheme interpreter
(define cont #f)
(call/cc (lambda (k)
           (set! cont k)
           (let loop ([exp (read)])
             (if (eq? exp 'end)
                 (cont 'bye)
                 (begin (write (eval exp))
                        (newline)
                        (loop (read)))))))