(load "environment.scm")

(define *lambda* (list 'lambda))
(define *primitive* (list 'primitive))

(define init-env
  (list (list
         (cons 'car              (list *primitive* 'car))
         (cons 'cdr              (list *primitive* 'cdr))
         (cons 'cadr             (list *primitive* 'cadr))
         (cons 'caddr            (list *primitive* 'caddr))
         (cons 'cadddr           (list *primitive* 'cadddr))
         (cons 'cddr             (list *primitive* 'cddr))
         (cons 'cons             (list *primitive* 'cons))
         (cons 'list             (list *primitive* 'list))
         (cons 'pair?            (list *primitive* 'pair?))
         (cons 'null?            (list *primitive* 'null?))
         (cons 'eq?              (list *primitive* 'eq?))
         (cons 'equal?           (list *primitive* 'equal?))
         (cons 'not              (list *primitive* 'not))
         (cons 'set-car!         (list *primitive* 'set-car!))
         (cons 'set-cdr!         (list *primitive* 'set-cdr!))
         (cons 'append           (list *primitive* 'append))
         (cons 'length           (list *primitive* 'length))
         (cons 'assq             (list *primitive* 'assq))
         (cons 'map              (list *primitive* 'map))
         (cons 'write            (list *primitive* 'write))
         (cons 'newline          (list *primitive* 'newline))
         (cons 'read             (list *primitive* 'read))
         (cons 'display          (list *primitive* 'display))
         (cons '+                (list *primitive* '+))
         (cons '-                (list *primitive* '-))
         (cons '*                (list *primitive* '*))
         (cons '/                (list *primitive* '/))
         (cons '=                (list *primitive* '=))
         (cons '>                (list *primitive* '>))
         (cons '<                (list *primitive* '<))
         (cons 'quotient         (list *primitive* 'quotient))
         (cons 'remainder        (list *primitive* 'remainder))
         (cons 'number?          (list *primitive* 'number?))
         (cons 'boolean?         (list *primitive* 'boolean?))
         (cons 'string?          (list *primitive* 'string?))
         (cons 'symbol?          (list *primitive* 'symbol?))
         (cons 'open-input-file  (list *primitive* 'open-input-file))
         (cons 'close-input-port (list *primitive* 'close-input-port))
         (cons 'eof-object?      (list *primitive* 'eof-object?)))))

(define (REPL)
  (display "i> ")
  (let ([r (read)])
    (if (and (pair? r) (eq? (car r) 'exit))
        (display "goodbye(^ ^)/")
        (begin
          (display "i: ")
          (REPL-print (base-eval r init-env))
          (newline)
          (REPL)))))

(define (REPL-print answer)
  (if (and (pair? answer) (eq? (car answer) *lambda*))
      (write (list (car answer) (cadr answer) (caddr answer)))
      (write answer)))

(define (base-eval exp env)
  (cond ((number? exp)  exp)
        ((boolean? exp) exp)
        ((string? exp)  exp)
        ((symbol? exp)           (eval-var exp env))
        ((eq? (car exp) 'and)    (eval-and exp env))
        ((eq? (car exp) 'begin)  (eval-begin exp env))
        ((eq? (car exp) 'cond)   (eval-cond exp env))
        ((eq? (car exp) 'define) (eval-define exp env))
        ((eq? (car exp) 'if)     (eval-if exp env))
        ((eq? (car exp) 'lambda) (eval-lambda exp env))
        ((eq? (car exp) 'let)    (eval-let exp env))
        ((eq? (car exp) 'load)   (eval-load exp env))
        ((eq? (car exp) 'quote)  (eval-quote exp env))
        ((eq? (car exp) 'set!)   (eval-set! exp env))
        (else                    (eval-application exp env))))

(define (eval-var exp env)
  (let ((pair (get exp env)))
    (if (pair? pair)
        (cdr pair)
        (error (list 'eval-var: 'unbound 'variable: exp)))))

(define (eval-define exp env) ;exp = (define var body) , (define (f x...) body)
  (let ((var (cadr exp))
        (body (cddr exp)))
    (if (pair? var)
        (let ([var (car var)]
              [body (cons 'lambda (cons (cdr var) body))])
          (let ([value (base-eval body env)])
            (define-value! var value env)))
        (let ([value (base-eval (car body) env)])
          (define-value! var value env)))))

(define (eval-lambda exp env) ;exp = (lambda params body)
  (let ((params (cadr exp))
        (body (cons 'begin (cddr exp))))
    (list *lambda* params body env)))

(define (eval-set! exp env) ;exp = (set! var body)
  (let* ((var (cadr exp))
         (body (caddr exp))
         (value (base-eval body env)))
    (set-value! var value env)))

(define (eval-let exp env) ;exp = (let ((p1 v1)...) body)
  (let* ([bindings (cadr exp)] ; = ((lambda (p1...) body) v1...)
         [params (map car bindings)]
         [values (map cadr bindings)]
         [body (cons 'begin (cddr exp))])
    (base-apply (list *lambda* params body env) values)))

(define (eval-quote exp env) ;exp = 'body = (quote body)
  (cadr exp))

(define (eval-if exp env) ;exp = (if pred t-clause f-clause)
  (let ((pred (cadr exp))
        (t-clause (caddr exp))
        (f-clause (cadddr exp)))
    (if (base-eval pred env)
        (base-eval t-clause env)
        (base-eval f-clause env))))

(define (eval-begin exp env) ;exp = (begin e1 e2 ...)
  (if (null? (cddr exp))
      (base-eval (cadr exp) env)
      (begin
        (base-eval (cadr exp) env)
        (eval-begin (cons 'begin (cddr exp)) env))))

(define (eval-and exp env) ; exp = (and e1 e2 ...)
  (let ([exps (cdr exp)])
    (cond [(null? exps) #t]
          [(null? (cdr exps)) (base-eval (car exps) env)]
          [else (if (base-eval (car exps) env)
                    (eval-and (cons 'and (cdr exps)) env)
                    #f)])))

(define (eval-cond exp env) ; exp = (cond (pred body ...) ... (else body ...))
  (let ([clause (cadr exp)])
    (let ([pred (car clause)] [bodies (cdr clause)])
      (if (eq? pred 'else)
          (base-eval (cons 'begin bodies) env)
          (if pred
              (base-eval (cons 'begin bodies) env)
              (base-eval (cons 'cond (cddr exp)) env))))))

(define (eval-application exp env) ;exp = (f a b c ...)
  (let ((ls (map (lambda (e) (base-eval e env))
                 exp)))
    (base-apply (car ls) (cdr ls))))

(define (base-apply operator operand)
  (cond ((and (pair? operator)
              (eq? (car operator) *primitive*))
         (let ((name (cadr operator)))
           (cond ((eq? name 'car)        (car (car operand)))
                 ((eq? name 'cdr)        (cdr (car operand)))
                 ((eq? name 'cadr)       (cadr (car operand)))
                 ((eq? name 'caddr)      (caddr (car operand)))
                 ((eq? name 'cadddr)     (cadddr (car operand)))
                 ((eq? name 'cddr)       (cddr (car operand)))
                 ((eq? name 'cons)       (cons (car operand) (cadr operand)))
                 ((eq? name 'list)       operand)
                 ((eq? name 'pair?)	 (pair? (car operand)))
                 ((eq? name 'null?)	 (null? (car operand)))
                 ((eq? name 'eq?)	 (eq? (car operand)
                                              (cadr operand)))
                 ((eq? name 'equal?)	 (equal? (car operand)
                                                 (cadr operand)))
                 ((eq? name 'not)	 (not (car operand)))
                 ((eq? name 'set-car!)	 (set-car! (car operand)
                                                   (cadr operand))
                                         '*unspecified*)
                 ((eq? name 'set-cdr!)	 (set-cdr! (car operand)
                                                   (cadr operand))
                                         '*unspecified*)
                 ((eq? name 'append)	 (append (car operand)
                                                 (cadr operand)))
                 ((eq? name 'length)	 (length (car operand)))
                 ((eq? name 'assq)	 (assq   (car operand)
                                                 (cadr operand)))
                 ((eq? name 'map)        (map (car operand) (cadr operand)))
                 ((eq? name 'write)	 (write (car operand)) '*unspecified*)
                 ((eq? name 'newline)	 (newline) '*unspecified*)
                 ((eq? name 'read)	 (if (null? operand)
                                             (read)
                                             (read (car operand))))
                 ((eq? name 'display)    (display (car operand)) '*unspecified*)
                 ((eq? name '+)		 (+ (car operand) (cadr operand)))
                 ((eq? name '-)		 (- (car operand) (cadr operand)))
                 ((eq? name '*)		 (* (car operand) (cadr operand)))
                 ((eq? name '/)		 (/ (car operand) (cadr operand)))
                 ((eq? name '=)		 (= (car operand) (cadr operand)))
                 ((eq? name '>)		 (> (car operand) (cadr operand)))
                 ((eq? name '<)		 (< (car operand) (cadr operand)))
                 ((eq? name 'quotient)	 (quotient (car operand)
                                                   (cadr operand)))
                 ((eq? name 'remainder)  (remainder (car operand)
                                                    (cadr operand)))
                 ((eq? name 'number?)	 (number? (car operand)))
                 ((eq? name 'boolean?)	 (boolean? (car operand)))
                 ((eq? name 'string?)	 (string? (car operand)))
                 ((eq? name 'symbol?)	 (symbol? (car operand)))
                 ((eq? name 'open-input-file)  (open-input-file (car operand)))
                 ((eq? name 'close-input-port) (close-input-port (car operand))
                                               '*unspecified*)
                 ((eq? name 'eof-object?)      (eof-object? (car operand)))
                 (else (error (list 'base-apply: 'unknown 'primitive: name))))))
        ((and (pair? operator)
              (eq? (car operator) *lambda*))
         (let ((lambda-params (cadr operator))
               (lambda-body (caddr operator))
               (lambda-env (cadddr operator)))
           (base-eval lambda-body
                      (extend lambda-env lambda-params operand))))
        (else (error (list 'base-apply: 'not 'a 'function: operator)))))

(define (eval-load exp env) ;exp = (load filename::String)
  (define port (open-input-file (cadr exp)))
  (define (load-local)
    (let ([input (read port)])
      (if (eof-object? input)
          (begin (close-input-port port) 'done)
          (begin (base-eval input env)
                 (load-local)))))
  (load-local))

(define (error symbol)
  (display symbol)
  (display " input return value> ")
  (read))

;(REPL)