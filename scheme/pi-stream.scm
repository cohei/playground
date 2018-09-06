(use util.stream) ; include srfi-40
(use srfi-43)

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (stream-display stream)
  (let loop ([t 11] [stream stream])
    (if (zero? t)
        (newline)
        (begin (display (stream-car stream))
               (newline)
               (loop (- t 1) (stream-cdr stream))))))

(define (scale-stream s c)
  (stream-map (lambda (x) (* x c)) s))

(define (partial-sums s)
  (stream-cons (stream-car s)
               (stream-map + (stream-cdr s)
                           (partial-sums s))))

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

(define (ksk s)
  (let ([s0 (stream-car s)]
        [s1 (stream-car (stream-cdr s))]
        [s2 (stream-car (stream-cdr (stream-cdr s)))])
    (stream-cons (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (* -2 s1) s2)))
                 (ksk (stream-cdr s)))))

(define (make-tableau transform s) ;; makes stream (s (t s) (t (t s)) ...)
  (stream-cons s
               (make-tableau transform (transform s))))

(stream-display (stream-map stream-car (make-tableau ksk pi-stream)))
