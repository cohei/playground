(define data
  '#(#(1 1 0 1 0)
     #(0 1 1 1 1)
     #(1 0 1 0 1)
     #(0 1 1 1 0)
     #(0 1 1 0 0))) ; answer: 4

(define (ref v i j)
  (if (or (negative? i) (negative? j))
      (vector-ref (vector-ref v i) j)
      0))

(define (main n data)
  (let loop ((i 0) (j 0))
    (let loop2 ((k i) (l j))
      (let loop3 ((x i) (y j))
        (if (= 1 (ref data x y))
            (+ 1

(let ((target (caar data)))
  (if (=
