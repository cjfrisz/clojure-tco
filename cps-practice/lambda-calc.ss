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
          [,v 
            (guard (symbol? v)) 
            (let ([k (new-var 'k)])
              `(lambda (,k) (,k ,v)))]
          [(lambda (,x) ,body)
           (let ([k (new-var 'k)])
             `(lambda (,k) (,k (lambda (,x) ,(cps body)))))]
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
;; Olivier Danvy (who seemingly picked it up from Sabry and Wadler) to
;; reduce the number of continuations by converting an expression e to
;; (lambda (k) E[[e]]) and the following transformations:
;; 	
;; 	E[[e]]			=	S[[e]] k
;; 				
;; 	S[[t]] K		=	K T[[t]]
;; 	S[[t0 t1]] K		=	T[[t0]] T[[t1]] K 
;; 	S[[t0 s1]] K		=	S[[s1]] (lambda (x1) S[[t0 x1]] K)
;; 	S[[s0 e1]] K		=	S[[s0]] (lambda (x0) S[[x0 e1]] K)
;; 				
;; 	T[[x]]			=	x
;; 	T[[(lambda (x) e)]]	=	(lambda (x) (lambda (k) E[[e]]))
;; 
;; We see here that it doesn't indiscriminantly CPS the application case
;; when one or both of the arguments is not serious by splitting the
;; transformation into three parts: the general expression CPSer, E, the
;; CPSer for "serious" expressions, S, and the transformer for trivial
;; expressions, T.
;; 
;; Of course, for the simple lambda calculus this is pretty
;; straightforward and follows our intuitions from above that both
;; variables and lambda expressions are simple and applications are
;; serious. The differences here are as follows:
;; 	- For the variable case, the expression is no longer wrapped
;; 	  in the continuation-applying lambda.
;; 	- For the function abstraction (lambda) case, the added
;; 	  continuation argument is not immediately applied to the
;; 	  inner expression. It should be noted for both this and the
;; 	  previous observation that while the transformation functions
;; 	  don't explicitly apply the continuations, this happens
;; 	  implicitly via the S transformer.
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
	(let ([new-var (string->symbol 
			(string-append (symbol->string sym) 
				       "."
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
;; P.S. The above notes are actually based off of looking at the
;; *wrong* section of Olivier's paper, though the code provided is
;; correct. The notes should be amended to reflect the transformation
;; that's *actually* used.

;; Well, sweet, we have a 50-line Scheme program that will take an
;; arbitrary lambda calculus expression and turn it into an equivalent
;; *CPSed* lambda calculus expression. So where do we go from here? As
;; cool as this is, the lambda calculus is kinda restrictive (we don't
;; even have constant, for glob's sake!), so it would be nice if we
;; could expand out a little bit. In fact, it would be super cool if
;; we could CPS arbitrary *Scheme* programs. But that takes quite a
;; bit of rote extension to defining what's simple and what's trivial,
;; and we don't want to worry with that just yet.
;;
;; Rather, one of the more interesting and practically useful
;; capabilities of Scheme (as well as Lisp and its various dialects)
;; is the ability to create arbitrary arity procedures. This isn't too
;; bad to implement, but it's not completely trivial, though. It
;; mostly reuses the machinery that we created for cps-olivier. The
;; only thing that really has significant change is the "S"
;; sub-procedure. Rather than assuming that applications occur in two
;; pieces, rator and rand, we assume that there can be any number of
;; elements in an application.
;;
;; Though this sounds complicated, it actually reduces the number of
;; cases that we need to observe. Instead of the 2! = 4 enumeration of
;; rator and rand either being trivial or serious, we assume that
;; there may have been some number of rator + (m - (k - 1)) operands before
;; the current rator/rand being observed and k operands after the one
;; being observed. For the ones prior to the current, it's assumed
;; that they've been taken care of; that is, trivials have been
;; ignored, while serious operands have been properly CPSed.

;;It's just nice to have the empty continuation handy for testing
(define empty-k (lambda (x) x))