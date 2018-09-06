(load "queue.scm")

(define process-queue (make-queue))

(define (coroutine thunk)
  (insert-queue! process-queue thunk))

(define (start)
  ((delete-queue! process-queue)))

(define (pause)
  (call/cc
   (lambda (k)
     (coroutine (lambda () (k #f)))
     (start))))

(coroutine (lambda ()
             (let loop ([i 0])
               (if (< i 10)
                   (begin
                     (display (+ 1 i))
                     (display " ")
                     (pause)
                     (loop (+ 1 i)))))))
(coroutine (lambda ()
             (let loop ([i 0])
               (if (< i 10)
                   (begin
                     (display (integer->char (+ i 97)))
                     (display " ")
                     (pause)
                     (loop (+ 1 i)))))))
(start)