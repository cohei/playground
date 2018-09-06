(require "./partcont")
(import kahua.partcont)

(define (power ls)
  (if (pair? ls)
      (let ((ls2 (power (cdr ls))))
        (append (map (lambda (x)
                       (cons (car ls) x))
                     ls2)
                ls2))
      (list ls)))

(define (power-aux p xs)
  (if (null? xs)
      p
      (power-aux (append p
                         (map (lambda (ys)
                                (cons (car xs) ys))
                              p))
                 (cdr xs))))
(define (pow xs)
  (power-aux '(()) xs))
