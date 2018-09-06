(define count 0)

(define (sum-countup5 x y) ;Int -> Int -> Countup Int
  (list (cons 'value (+ x y))
        (cons 'countup 5)))
(define (sum-countup5+ x y) ;Countup Int -> Countup Int -> Countup Int
  (list (cons 'value (+ (cdr (assq 'value x))
                        (cdr (assq 'value y))))
        (cons 'countup 5)))

(define (length-countup str) ;String -> Countup Int
  (let ((l (string-length str)))
    (list (cons 'value l)
          (cons 'countup l))))
(define (length-countup+ str) ;Countup String -> Countup Int
  (let ((l (string-length (cdr (assq 'value str)))))
    (list (cons 'value l)
          (cons 'value l))))

(define (countup n) ;Int -> Countup ()
  (list (cons 'value '<undefined>)
        (cons 'countup n)))
(define (countup+ n) ;Countup Int -> Countup ()
  (list (cons 'value '<undefined>)
        (cons 'countup (cdr (assq 'value n)))))

(define (countup-main countup-req) ;Countup a -> a + sideeffect
  (set! count (+ count (cdr (assq 'countup countup-req))))
  (cdr (assq 'value countup-req)))

(define (return a) ;a -> Countup a
  (list (cons 'value a)
        (cons 'countup 0)))

(define (ext f) ;(a -> Countup b) -> (Countup a -> Countup b)
  (
