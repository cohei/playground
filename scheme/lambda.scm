(define (variable? x)
  (and (not (null? x)) (symbol? x)))
(define (application? x)
  (and (pair? x) (pair? (cdr x)) (not (cddr x))))
(define (abstraction? x)
  (and (pair? x) (pair? (cdr x)) (pair? (cddr x)) (not (cdddr x))
       (eq? '*lambda* (car x)) (variable? (cadr x))))

(define-syntax if-let1
  (syntax-rules ()
    ((_ var test then else)
     (or (and-let* ((var test)) then)
         else))))


; abstraction -> (lambda x (x x))
; application -> (() ())

(define (substitute x al)
  (cond
   ((variable? x) (if-let1 val (assoc x al) (cdr val) x))
   ((application? x) (list (substitute (car x) al) (substitute (cadr x) al)))
   ((abstraction? x)
    ((lambda (abs val)
       (let ((var (car abs)) (expr (cadr abs)))
         (list '*lambda* val (substitude expr (cons (cons var val) al)))))
     (cdr x) (gensym)))))

(define (beta? x)
  (and (application? x) (abstraction? (car x))))
(define (beta x)
  (cond
   ((beta? x) (let (
