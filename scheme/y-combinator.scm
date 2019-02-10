(define Y
  (lambda (f)
    ((lambda (m)
       (f (lambda (a) ((m m) a))))
     (lambda (m)
       (f (lambda (a) ((m m) a)))))))

(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))
