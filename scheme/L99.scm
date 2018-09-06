(define test-list '(a b c d))

(define (p01 ls)
  (let1 ls1 (cdr ls)
    (if (null? ls1)
        ls
        (p01 ls1))))

(define (p02 ls)
  (if (null? (cddr ls))
      ls
      (p02 (cdr ls))))

(define (p03 ls n) ; start from 1
  (if (= n 1)
      (car ls)
      (p03 (cdr ls) (- n 1))))

(define (p04 ls)
  (let1 ones (map (lambda (_) 1) ls)
    (apply + ones)))

(define (p05 ls)
  (cond
   ((null? ls) '())
   ((null? (cdr ls)) ls)
   (else (append (p05 (cdr ls)) (list (car ls))))))

(define (p06 ls)
  (equal? ls (reverse ls)))

(define (p07 ls)
  (cond
   ((null? ls) '())
   ((not (pair? (car ls)))
    (cons (car ls) (p07 (cdr ls))))
   (else
    (append (p07 (car ls)) (p07 (cdr ls))))))

(define (p08 ls)
  (cond
   ((null? ls) '())
   ((null? (cdr ls)) ls)
   ((eqv? (car ls) (cadr ls))
    (p08 (cdr ls)))
   (else
    (cons (car ls) (p08 (cdr ls))))))

(define (takeWhile p ls)
  (cond
   ((null? ls) '())
   ((p (car ls))
    (cons (car ls) (takeWhile p (cdr ls))))
   (else (takeWhile p (cdr ls)))))

(define (p09 ls)
  (cond
   ((null? ls) '())
   ((null? (cdr ls))
    (list ls))
   ((equal? (car ls) (cadr ls))

    (takeWhile (lambda (x) (equal? x (car x))) ls)
