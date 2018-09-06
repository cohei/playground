(define (make-env) (list '()))

(define (define-value! <var> <value> <env>)
  (if (assq <var> (car <env>))
      (set-cdr! (assq <var> (car <env>)) <value>)
      (let ([new (list (cons <var> <value>))])
        (set-cdr! new (car <env>))
        (set-car! <env> new))))

(define (set-value! <var> <value> <env>)
  (if (get <var> <env>)
      (set-cdr! (get <var> <env>) <value>)
      (define-value! <var> <value> <env>)))

(define (get <var> <env>)
  (if (null? <env>)
      #f
      (if (assq <var> (car <env>))
          (assq <var> (car <env>))
          (get <var> (cdr <env>)))))

(define (extend <env> <params> <args>)
  (cons (map cons <params> <args>) <env>))
