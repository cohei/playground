(define (fold-left f b ls)
  (if (null? ls)
      b
      (fold-left f (f b (car ls)) (cdr ls))))
(define-syntax list-of
  (syntax-rules ()
    ((_ expr ...)
     (reverse (list-of-tail '() expr ...)))))
(define-syntax list-of-tail
  (syntax-rules ()
    ((_ base expr)
     (cons expr base))
    ((_ base expr (var in generator) rest ...)
     (let* ((f (lambda (z var) (list-of-tail z expr rest ...))))
       (fold-left f base generator)))
    ((_ base expr pred? rest ...)
     (if pred?
         (_ base expr rest ...)
         base))))

(define (qsort lt? ls)
  (if (null? ls)
      '()
      (append
       (qsort lt? (list-of x (x in (cdr ls)) (lt? x (car ls))))
       (list (car ls))
       (qsort lt? (list-of x (x in (cdr ls)) (not (lt? x (car ls))))))))
