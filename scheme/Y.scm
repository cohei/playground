;((lambda (n)
;   (if (zero? n)
;       1
;       (* n (fact (- n 1)))))
; 10)

((lambda (f n)
   (if (zero? n)
       1
       (* n (f f (- n 1)))))
 (lambda (f n)
   (if (zero? n)
       1
       (* n (f f (- n 1)))))
 10)

((lambda (f)
   (lambda (n)
     (if (zero? n)
         1
         (* n ((f f) (- n 1))))))
 (lambda (f)
   (lambda (n)
     (if (zero? n)
         1
         (* n ((f f) (- n 1)))))))

((lambda (f)
   (lambda (n)
     (if (zero? n)
         1
         (* n ((lambda (a) ((f f) a)) (- n 1))))))
 (lambda (f)
   (lambda (n)
     (if (zero? n)
         1
         (* n ((lambda (a) ((f f) a)) (- n 1)))))))

((lambda (f)
   ((lambda (r)
      (lambda (n)
        (if (zero? n)
            1
            (* n (r (- n 1))))))
    (lambda (a) ((f f) a))))
 (lambda (f)
   ((lambda (r)
      (lambda (n)
        (if (zero? n)
            1
            (* n (r (- n 1))))))
    (lambda (a) ((f f) a)))))

((lambda (m)
   ((lambda (f) (m (lambda (a) ((f f) a))))
    (lambda (f) (m (lambda (a) ((f f) a))))))
 (lambda (r)
   (lambda (n)
     (if (zero? n)
         1
         (* n (r (- n 1)))))))

(define Y
  (lambda (m) ((lambda (f) (m (lambda (a) ((f f) a))))
               (lambda (f) (m (lambda (a) ((f f) a)))))))
(define fact
  (lambda (r)
    (lambda (n)
      (if (zero? n)
          1
          (* n (r (- n 1)))))))

((Y fact) 10)
