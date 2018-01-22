(define primitive-environment
  `((apply . ,apply) (assq . ,assq) (call/cc . ,call/cc) (car . ,car) (cadr . ,cadr) (caddr . ,caddr) (cadddr . ,cadddr) (cddr . ,cddr) (cdr . ,cdr) (cons . ,cons) (eq? . ,eq?) (list . ,list) (map . ,map) (memv . ,memv) (null? . ,null?) (pair? . ,pair?) (read . ,read) (set-car! .,set-car!) (set-cdr! . ,set-cdr!) (symbol? . ,symbol?)))

(define new-env
  (lambda (formals actuals env)
    (cond
     ((null? formals) env)
     ((symbol? formals) (cons (cons formals actuals) env))
     (else
      (cons (cons (car formals) (car actuals))
	    (new-env (cdr formals) (cdr actuals) env))))))

(define lookup
  (lambda (var env)
    (cdr (assq var env))))

(define assign
  (lambda (var val env)
    (set-cdr! (assq var env) val)))

(define exec
  (lambda (exp env)
    (cond
     ((symbol? exp) (lookup exp env))
     ((pair? exp)
      (case (car exp)
	((quote) (cadr exp))
	((lambda)
	 (lambda vals
	   (let ((env (new-env (cadr exp) vals env)))
	     (let loop ((exps (cddr exp)))
	       (if (null? (cdr exps))
		   (exec (car exps) env)
		   (begin
		     (exec (car exps) env)
		     (loop (cdr exps))))))))
	((if)
	 (if (exec (cadr exp) env)
	     (exec (caddr exp) env)
	     (exec (cadddr exp) env)))
	((set!)
	 (assign (cadr exp)
		 (exec (caddr exp) env)
		 env))
	((let)
	 (let ((names (map car (cadr exp)))
	       (vals (map (lambda (x) (exec (cadr x) env)) (cadr exp))))
	   (let ((env (new-env names vals env)))
	     (exec (caddr exp) env))))
;	((begin)
;	 (begin
;	   (adr exp)
	(else
	 (apply (exec (car exp) env)
		(map (lambda (x) (exec x env))
		     (cdr exp))))))
     (else exp))))

(begin
  (define x 10)
  x)

(interpret `(let ((i 'a))
	      (cons i i)))
(interpret `(cons 'a 'a))
(interpret `(let ((i 'a) (j 'b))
	      (cons i j)))
(interpret `(let ((i 1))
	      (let ((j (cons i 'b)))
		(list i j))))

(define exp '((i 'a) (j 'b)))
(new-env (map car exp) (map cadr exp) '())
(map cdr (new-env (map car exp) (map cadr exp) '()))


(define interpret
  (lambda (exp)
    (exec exp primitive-environment)))

(interpret `(cons 3 4))
(interpret
 `((lambda (x . y)
     (list x y))
   'a 'b 'c 'd))
(interpret
 `(((call/cc (lambda (k) k))
    (lambda (x) x))
   "HEY!"))
(interpret `((lambda (memq)
	       (memq memq 'a '(b c a d e)))
	     (lambda (memq x ls)
	       (if (null? ls) #f
		   (if (eq? (car ls) x)
		       ls
		       (memq memq x (cdr ls)))))))
(interpret
 `((lambda (reverse)
     (set! reverse
	   (lambda (ls new)
	     (if (null? ls)
		 new
		 (reverse (cdr ls) (cons (car ls) new)))))
   (reverse '(a b c d e) '()))
   #f))

