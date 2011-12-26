;; Ok, let's tackle CPSing the lambda calculus.
;; We start with this grammar:
;;	E := v			-- variables
;;	   | (lambda (x) E)	-- function abstraction (lambda)
;;	   | (E1 E2)		-- application
;;
;; According to our handy CPSing rules (thanks, B522), these get transformed to the following:
;;	(cps v)			=> (lambda (k) (k v))
;;	(cps (lambda (x) E))	=> (lambda (k) 
;;				     (k (lambda (x) 
;;					  (cps E))))
;;	(cps (E1 E2)		=> (lambda (k) 
;;				     ((cps E1) (lambda (v1) 
;;						 ((cps E2) (lambda (v2) 
;;							     ((v1 v2) k))))))
;;
;; This should be a pretty straight-forward pmatch-style transformation
(load "pmatch.scm")

(define dumb-cps
  (lambda (e)
    (define var-num (make-parameter 0))
    (define new-var
      (lambda (sym)
	(let ([new-var (string->symbol 
                         (string-append (symbol->string sym) 
                           "."
                           (number->string (var-num))))])
	  (begin
	    (var-num (add1 (var-num)))
	    new-var))))
    (define cps
      (lambda (e)
        (pmatch e
          [,v (guard (symbol? v)) (let ([k (new-var 'k)])
                                    `(lambda (,k) (,k ,v)))]
          [(lambda (,x) ,body) (let ([k (new-var 'k)])
                                 `(lambda (,k)
                                    (,k (lambda (,x)
                                          ,(cps body)))))]
          [(,rator ,rand)
           (let ([k (new-var 'k)]
                 [v1 (new-var 'v)]
                 [v2 (new-var 'v)])
             `(lambda (,k) 
                (,(cps rator) 
                 (lambda (,v1) 
                   (,(cps rand) 
                    (lambda (,v2) 
                      ((,v1 ,v2) ,k)))))))]
          [else (error 'dumb-cps "Invalid lambda calculus expression ~s."
                  e)])))
    (cps e)))

;; As you might tell from the naming above, this isn't a great
;; solution for real CPS. The problem is that it introduces a whole
;; bunch of unnecessary continuations, popularly known in the
;; literature as "administrative redexes." This can increase the
;; amount of memory necessary for the program thanks to the additional
;; closures and decrease performance because of additional function
;; invocations. Of course, more theory-minded people argue that "this
;; just looks bad." Rather, we can use a smarter CPS transform via
;; Olivier Danvy to reduce the number of continuations by converting
;; an expression e to (lambda (k) E[[e]]) and the following
;; transformations:
;;
;;      E[[t]]                  =       (k T[[t]])
;; 	E[[s]]			=	(S[[s]] k)
;; 				
;;      S[[(t0 t1)]]            =       ((T[[t0]] T[[t1]]) K)
;;      S[[(t0 s1)]]            =       (S[[s1]] (lambda (x1)
;;                                                (T[[t0]] x1) K))
;;      S[[(s0 t1)]]            =       (S[[s0]] (lambda (x0)
;;                                                ((x0 T[[t1]]) K)))
;;      S[[(s0 s1)]]            =       (S[[s0]]
;;                                        (lambda (x0)
;;                                          (S[[s1]] (lambda (x1)
;;                                                     ((x0 x1) K)))))
;;
;; 				
;; 	T[[x]]			=	x
;; 	T[[(lambda (x) e)]]	=	(lambda (x)
;;                                        (lambda (k)
;;                                          E[[e]]))
;; 
;; We see here that it doesn't indiscriminantly CPS the application
;; case when one or both of the arguments is not serious by splitting
;; the transformation into three parts: the general expression CPSer,
;; E, the CPSer for "serious" expressions, S, and the transformer for
;; trivial expressions, T.
;; 
;; Of course, for the simple lambda calculus this is pretty
;; straightforward and follows our intuitions from above that both
;; variables and lambda expressions are simple and applications are
;; serious. The differences here are as follows:
;; 	- For the variable case, the expression is no longer wrapped
;; 	  in the continuation-applying lambda. This is implicitly
;; 	  taken care of by the outer E.
;; 	- For the function abstraction (lambda) case, the added
;; 	  continuation argument is not immediately applied to the
;; 	  inner expression. Again, this happens inside the inner call
;; 	  to E.
;; 	- The application case discerns between trivial and serious
;; 	  operators and operands, only adding new continuations for
;; 	  serious rators/rands
;; 
;; This suggests that we reuse the bits of the machinery from above for
;; creating unique variables and write the three new functions for
;; applying the correct CPS transformations. Additionally it would be
;; useful to have pmatch-using predicates for determining if a given
;; expression is simple or serious. Well, with all that explanation
;; behind us, let's forge ahead:
(define cps-olivier
  (lambda (e)
    (define var-num (make-parameter 0))
    (define new-var
      (lambda (sym)
	(let ([new-var
                (string->symbol (string-append (symbol->string sym) "."
                                  (number->string (var-num))))])
	  (begin
	    (var-num (add1 (var-num)))
	    new-var))))
    (define trivial?
      (lambda (t)
	(pmatch t
          [,t (guard (symbol? t)) #t]
	  [(lambda (,x) ,body) #t]
	  [else #f])))
    (define E
      (lambda (e k)
	(if (trivial? e)
            `(,k ,(T e))
            (S e k))))
    (define S
      (lambda (e k)
	(pmatch e 
	  [(,t.0 ,t.1) 
	   (guard (and (trivial? t.0) (trivial? t.1))) 
	   `((,(T t.0) ,(T t.1)) ,k)]
	  [(,t.0 ,s.1) 
	   (guard (trivial? t.0))
	   (let ([x.1 (new-var 'x)])
	     (S s.1 `(lambda (,x.1) ((,(T t.0) ,x.1) ,k))))]
          [(,s.0 ,t.1)
           (guard (trivial? t.1))
           (let ([x.0 (new-var 'x)])
             (S s.0 `(lambda (,x.0) ((,x.0 ,(T t.1)) ,k))))]
          [(,s.0 ,s.1)
           (let ([x.0 (new-var 'x)] [x.1 (new-var 'x)])
             (S s.0 `(lambda (,x.0)
                       ,(S s.1 `(lambda (,x.1)
                                  ((,x.0 ,x.1) ,k))))))])))
    (define T
      (lambda (e)
	(pmatch e
	  [,x (guard (symbol? x)) x]
	  [(lambda (,x) ,body)
	   (let ([k (new-var 'k)])
	     `(lambda (,x) (lambda (,k) ,(E body k))))])))
    (let ([k (new-var 'k)])
      `(lambda (,k) ,(E e k)))))

;; Well, sweet, we have a 50-line Scheme program that will take an
;; arbitrary lambda calculus expression and turn it into an equivalent
;; *CPSed* lambda calculus expression. So where do we go from here? As
;; cool as this is, the lambda calculus is kinda restrictive (we don't
;; even have constants, for glob's sake!), so it would be nice if we
;; could expand out a little bit. In fact, it would be super cool if
;; we could CPS arbitrary *Scheme* programs. But that takes quite a
;; bit of rote extension to defining what's simple and what's trivial,
;; and we don't want to worry with that just yet.
;;
;; Rather, one of the more interesting and practically useful
;; capabilities of Scheme (as well as Lisp and its various dialects)
;; is the ability to create arbitrary arity procedures. This isn't too
;; bad to implement, but it's not completely trivial, either. It
;; mostly reuses the machinery that we created for cps-olivier. The
;; only thing that really has significant change is the "S"
;; sub-procedure. Rather than assuming that applications occur in two
;; pieces, rator and rand, we assume that there can be any number of
;; elements in an application.
;;
;; Though this sounds complicated, it actually reduces the number of
;; cases that we need to observe. Instead of the 2! = 4 enumeration of
;; rator and rand either being trivial or serious, we assume that
;; there may have been some number of rator + (m - (k - 1)) operands
;; before the current rator/rand being observed and k operands after
;; the one being observed. For the ones prior to the current, it's
;; assumed that they've been taken care of; that is, trivials have
;; been ignored, while serious operands have been properly CPSed.
;;
;; So let's run through a few examples of this before we dig in with
;; the code. Let's start with something pretty simple:
;;
;; (a b c)
;; => ((a b c) k)
;;
;; ((a b) c)
;; => ((a b) (lambda (s.0) (s.0 c k)))
;;
;; (a (b c) (d e))
;; => ((b c) (lambda (s.0) ((d e) (lambda (s.1) ((a s.0 s.1) k)))))
;;
;; So we see from these few examples that for each serious item in the
;; procedure call list (either in the operator position or one of the
;; operands), we evaluate it first, add a lambda to the continuation
;; to bind the evaluated value for the serious item, and replace its
;; occurrence in the original call with the variable to which its
;; bound. This application with all the serious items replaced by
;; variables is used in the innermost part of continuation.
;;
;; In essence, we want the following to happen:
;;      1. Pull off the car of the list.
;;      2. Evaluate the rest of the procedure call list.
;;      3. Determine whether the car we pulled in (1) is trivial or
;;         serious.
;;              3.a. If it's trivial, we want to evaluate it as such
;;                   and make sure it gets back in its original place
;;                   in the procedure call in the innermost part of the
;;                   continuation.
;;              3.b. If the is serious, we want to return an
;;                   expression with the following properties:
;;                      i.   The serious expression (CPSed) is
;;                           evaluated first relative to the original
;;                           application.
;;                      ii.  It is applied to a continuation that 
;;                           binds its evaluated form to a variable,
;;                           x.
;;                      iii. The serious expression in the original
;;                           application is replaced by x in the
;;                           continuation. 
(define (n-arity-cps e)
  (define var-num (make-parameter 0))
  (define (new-var sym)
    (let ([new-var (string->symbol 
                     (string-append (symbol->string sym) 
                       "."
                       (number->string (var-num))))])
      (begin
        (var-num (add1 (var-num)))
        new-var)))
  (define (trivial? t)    
    (pmatch t
      [,t (guard (symbol? t)) #t]
      [(lambda (,fst . ,rst) ,body) #t]
      [else #f]))
  (define (E e k)
    (if (trivial? e)
        `(,k ,(T e))
        (S e (lambda (call) `(,call ,k)))))
  (define (S e k)
    (let ([fst (or (null? e) (car e))] [rest (or (null? e) (cdr e))])
      (cond
        [(null? e) (k '())]
        [(trivial? fst) (let ([fst (T fst)])
                          (S rest (lambda (n) (k (cons fst n)))))]
        [else (let ([s (new-var 's)])
                (S fst (lambda (n)
                         `(,n (lambda (,s)
                                  ,(k (S rest
                                        (lambda (r)
                                          (cons s r)))))))))])))
  (define (T e)
    (pmatch e
      [,x (guard (symbol? x)) x]
      [(lambda (,fst . ,rst) ,body)
       (let ([k (new-var 'k)])
         `(lambda (,fst . ,rst) (lambda (,k) ,(E body k))))]))
  (let ([k (new-var 'k)])
    `(lambda (,k) ,(E e k))))

;;It's just nice to have the empty continuation handy for testing
(define empty-k (lambda (x) x))