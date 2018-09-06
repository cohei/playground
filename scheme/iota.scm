(define (iota)
  (if (eq? #\* (read-char))
      ((iota) (iota))
      (lambda (c)
        (c S K))))

(define S
  (lambda (x)
    (lambda (y)
      (lambda (z)
        ((x z) (y z))))))

(define K
  (lambda (x)
    (lambda (y)
      x)))
