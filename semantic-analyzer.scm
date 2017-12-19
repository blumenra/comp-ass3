(define remove-applic-lambda-nil
    (lambda (parseExp)
        (cond ((or (null? parseExp) (not (pair? parseExp))) parseExp)
            ((and (equal? (car parseExp) 'applic)
                (equal? (caadr parseExp) 'lambda-simple)
                (equal? (cadadr parseExp) '()))
                (remove-applic-lambda-nil (car (cddadr parseExp))))
                (else (map remove-applic-lambda-nil parseExp)))))

(define inner-is-bound
    (lambda (var exp counter)
        (cond ((or (null? exp) (not (pair? exp))) #f)
            ((and 
                (equal? exp (list 'var var))
                (> counter 0)) #t)
            ((equal? (car exp) 'lambda-simple)
                (if (member var (cadr exp)) #f
                    (ormap (lambda (x) (inner-is-bound var x (+ 1 counter))) (cddr exp))))
            (else (ormap (lambda (x) (inner-is-bound var x counter)) exp)))))
        
(define is-bound?
    (lambda (var exp)
        (inner-is-bound var exp 0)))
            
(define is-set?
    (lambda (var exp)
      (cond ((or (null? exp) (not (pair? exp))) #f)
        ((and
            (equal? (car exp) 'set)
            (equal? (cadadr exp) var)) #t)
        (else (ormap (lambda (x) (is-set? var x)) exp)))))
        
(define is-get?
    (lambda (var exp)
        (cond ((or (null? exp) (not (pair? exp))) #f)
              ((equal? exp (list 'var var)) #t) 
              ((equal? (car exp) 'set) (is-get? var (cddr exp)))
              ((and (equal? (car exp) 'lambda-opt) 
                    (or (equal? var (caddr exp))
                        (member var (cadr exp))) #f))
              ((and (equal? (car exp) 'lambda-simple) (member var (cadr exp))) #f)
              (else (ormap (lambda (x) (is-get? var x)) exp))))) 

(define to-box?
    (lambda (var exp)
        (and 
            (is-bound? var exp)
            (is-set? var exp) 
            (is-get? var exp))))
            
(define boxing
    (lambda (var exp)
        (if (to-box? var exp)
            (append `(seq ((set (var ,var) (box (var ,var)))))
            (to-box-set var (to-box-get var exp))))
            ))
            
(define to-box-get 
  (lambda (var exp)
        (cond ((or (null? exp) (not (pair? exp))) exp)
              ((equal? exp (list 'var var)) `(box-get (var ,var))) 
              ((equal? (car exp) 'set) `(set ,(to-box-get var (cddr exp))))
              ((and (equal? (car exp) 'lambda-opt) 
                    (or (equal? var (caddr exp))
                        (member var (cadr exp))) exp))
              ((and (equal? (car exp) 'lambda-simple) (member var (cadr exp))) exp)
              (else (map (lambda (x) (to-box-get var x)) exp))))) 

(define to-box-set
    (lambda (var exp)
      (cond ((or (null? exp) (not (pair? exp))) exp)
        ((and
            (equal? (car exp) 'set)
            (equal? (cadadr exp) var)) `(box-set ,@(cdr exp)))
        (else (map (lambda (x) (to-box-set var x)) exp)))))
        
(define box-set
  ;; fill in the variable boxing details here
  )

(define pe->lex-pe
  ;; fill in the lexical addressing details here
  )

(define annotate-tc
  ;; fill in the tail-call annotation details here
  )
