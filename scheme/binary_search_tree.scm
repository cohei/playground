(define-struct tree (root compare))
(define-struct node (value left right)) ;leaf no leftright ha #f tosuru

(define (number-compare n1 n2)
  (cond [(< n1 n2) -1]
        [(= n1 n2) 0]
        [else 1]))

(define mytree (make-tree (make-node 0 #f #f) number-compare))

(define (tree-check obj)
  (if (not (tree? obj))
      (error "provided not tree")))

(define (tree-search root value)
  (tree-check root)
  (let ((node (tree-root root))
        (func (tree-compare root)))
    (if (null? node)
        'not-found
        (case (func value (node-value node))
          ((0) 'found)
          ((1) (tree-search (node-right node) value))
          (else (tree-search (node-left node) value))))))

(define (tree-insert! root value)
  (let ([compare (tree-compare root)])
    (set-tree-root! root
                    (let sub ([node (tree-root root)])
                      (if node
                          (case (compare value (node-value node))
                            [(1)  (set-node-right! node (sub (node-right node)))]
                            [(-1) (set-node-left!  node (sub (node-left  node)))])
                          (set! node (make-node value #f #f)))
                      node))))

(define (tree-print tree)
  (tree-check tree)
  (let sub ([node (tree-root tree)])
    (when node
      (sub (node-left node))
      (print (node-value node))
      (display " ")
      (sub (node-right node)))))

(define (ra tree n)
  (let ([2n (* 2 n)])
    (let sub ([n n])
      (if (= n 0)
          'done
          (begin (tree-insert! tree (random 2n))
                 (sub (- n 1)))))))

(ra mytree 1000)
