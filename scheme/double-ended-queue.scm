(load "queue.scm")

(define make-deq make-queue)

(define empty-deq? empty-queue?)

(define (front deq)
  (if (empty-deq? deq)
      (error "front: empty double-ended queue")
      (caar deq)))

(define (rear deq)
  (if (empty-deq? deq)
      (error "rear: empty double-ended queue")
      (cdr deq)))

(define (front-insert-deq! deq x)
  (let ([new-pair (cons x '())])
    (if (empty-deq? deq)
        (insert-queue! deq x)
        (begin (set-cdr! new-pair (car deq))
               (set-car! deq new-pair)))))

(define rear-insert-deq! insert-queue!)

(define (front-delete-deq! deq)
  (if (empty-deq? deq)
      (error "front-delete-deq!: empty double-ended queue")
      (set-car! deq (cdar deq))))

(define (rear-delete-deq! deq)
  (if (empty-deq? deq)
      (error "rear-delete-deq!: empty double-ended queue")
      (let loop ([ls (car deq)])
        (if (null? (cddr ls))
            (begin (set-cdr! ls '())
                   (set-cdr! deq ls))
            (loop (cdr ls))))))

(define (print-deq deq)
  (define (sub-print ls)
    (if (null? ls)
        (begin (display " <-> rear\n")
               'done)
        (begin (display " <-> ")
               (write (car ls))
               (sub-print (cdr ls)))))
  (display "front")
  (sub-print (car deq)))