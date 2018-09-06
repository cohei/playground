(define tree1 '(1 (2 3) 4 (5 (6 7))))
(define tree2 '((1 2 ((3 4) 5)) 6 7))

(define (find-leaf obj tree)
  (call/cc
   (lambda (k)
     (let loop ([tree tree])
       (cond
         [(null? tree) #f]
         [(pair? tree) (loop (car tree)) (loop (cdr tree))]
         [(eq? obj tree) (k obj)]
         [else 'error])))))

(define-syntax block
  (syntax-rules ()
    ((_ tag body ...)
     (call/cc (lambda (tag) body ...)))))

(define (leaf-generator tree)
  (let ([return '()])
    (letrec ([continue
              (lambda (x)
                (let loop ([tree tree])
                  (cond
                    [(null? tree) 'skip]
                    [(pair? tree) (loop (car tree)) (loop (cdr tree))]
                    [else (call/cc (lambda (lap-to-go)
                                     (set! continue lap-to-go)
                                     (return tree)))]))
                (return '()))])
      (lambda ()
        (call/cc (lambda (where-to-go)
                   (set! return where-to-go)
                   (continue 'start)))))))

(define (tree-compare t1 t2)
  (let ([lg1 (leaf-generator t1)]
        [lg2 (leaf-generator t2)])
    (let loop ([leaf1 (lg1)] [leaf2 (lg2)])
      (if (and (null? leaf1) (null? leaf2))
          #t
          (if (eq? leaf1 leaf2)
              (loop (lg1) (lg2))
              #f)))))