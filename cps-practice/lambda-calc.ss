;;----------------------------------------------------------------------
;; File lambda-calc.ss
;; Written by Chris Frisz
;; 
;; Created 3 Dec 2011
;; Last modified  8 Jan 2012
;; 
;; The file lambda-calc.ss contains several modules for CPSing lambda
;; calculus expressions. They are as follows:
;;      dumb-cps:
;;              Performs a general CPS transformation on a standard
;;              lambda calculus expression. This is a "dumb"
;;              transformation because it introduces unnecessary
;;              administrative redexes that are eliminated in more
;;              sophisticated CPS methods.
;;      olivier-cps:
;;              Performs a first-order one-pass CPS transformation on
;;              a standard lambda calculus expression as shown in
;;              Olivier Danvy's (appropriately-named) "A First-Order
;;              One-Pass CPS Transformation."
;;      n-arity-cps:
;;              Performs a CPS transformation in the style of the
;;              Olivier transformation used in the olivier-cps
;;              module, with a twist. Rather than only operating on
;;              standard lambda calculus expressions, which may either
;;              be symbols, lambda expressions, or two-part, operator
;;              and operand function applications, it transforms
;;              expressions with procedures of arbitrary arity.
;; Each one of these modules exports a single procedure, "cps," which
;; performs the CPS transformation on input lambda calculus
;; expressions according to the method associated with the module (as
;; described above).
;;
;; Note, this file requires the file "pmatch.scm" for the pmatch
;; macro.
;;----------------------------------------------------------------------

;; This is gonna need pmatch
(load "pmatch.scm")

;; Just some stuff to make writing CPSers easier This includes a
;; procedure, new-var, for generating a unique variable with the input
;; symbol as a base, a predicate, trivial?, for determining whether an
;; input lambda calculus expression is trivial, and the empty
;; continuation, empty-k.
(module cps-helpers (new-var trivial? empty-k)

  ;; Use an essentially global counter to ensure that each variable is
  ;; unique.
  (define var-num (make-parameter 0))

  ;; The new-var procedure does the actual work of generating the
  ;; unique variables. It takes a symbol and appends the current value
  ;; of var-num to the symbol (separated by a '.') to generate the
  ;; variable. This method is based on the method used to generate
  ;; unique symbols for CSCI-P423
  (define new-var
    (lambda (sym)
      (let ([new-var (string->symbol 
                       (string-append (symbol->string sym) 
                         "."
                         (number->string (var-num))))])
        (begin
          (var-num (add1 (var-num)))
          new-var))))
  
  ;; The predicate trivial? returns a boolean representing whether an
  ;; input lambda calculus expression (including lambda expressions of
  ;; arbitrary arity) is trivial. 
  (define trivial?
    (lambda (t)
      (pmatch t
          [,t (guard (symbol? t)) #t]
	  [(lambda ,fmls ,body) #t]
	  [else #f])))
  
  ;;It's just nice to have the empty continuation handy for testing
  (define empty-k (lambda (x) x))
)

(module lambda-calc-verify (verify-lambda-calc verify-lambda-calc-na)

  ;; The verify-lambda-calc procedure verifies a standard lambda
  ;; calculus expression, returning #t if the input is a
  ;; correctly-formed lambda calculus expression and returns an error
  ;; otherwise.
  (define (verify-lambda-calc e)
    (pmatch e
      [,x (guard (symbol? x)) #t]
      [(lambda (,x) ,body) (verify-lambda-calc body)]
      [(,rator ,rand) (and (verify-lambda-calc rator)
                           (verify-lambda-calc rand))]
      [else (errorf 'verify-lambda-calc
                   "Invalid lambda calculus expression ~s"
                   e)]))

  ;; The verify-lambda-calc-na procedure verifies a lambda calculus
  ;; expression where procedures may take an arbitrary number of
  ;; arguments. 
  (define (verify-lambda-calc-na e)
    (pmatch e
      [,x (guard (symbol? x)) #t]
      [(lambda (,fst . ,rst) ,body) (verify-lambda-calc body)]
      ;; We make a special case for applying lambda expressions to
      ;; verify an appropriate number of arguments.
      [((lambda ,fmls ,body) ,a1 . ,ar)
       (if (= (length `(,a1 . ,ar)) (length fmls))
           (and (verify-lambda-calc-na body)
                (for-each verify-lambda-calc-na `(,a1 . ,ar)))
           (errorf 'verify-lambda-calc-na
                  "Mismatched number of formals and arguments in ~s"
                  e))]
      [(,rator . ,rand) (and (verify-lambda-calc-na rator)
                             (for-each verify-lambda-calc-na rand))]
      [else (errorf 'verify-lambda-calc-na
                   "Invalid lambda calculus expression ~s"
                   e)]))
)


;; Ok, let's tackle CPSing the lambda calculus.
;; We start with this grammar:
;;	E := v			-- variables
;;	   | (lambda (x) E)	-- function abstraction (lambda)
;;	   | (E1 E2)		-- application
;;
;; According to our handy CPSing rules (thanks, B522), these get
;; transformed to the following:
;;	(cps v)			=> (lambda (k) (k v))
;;	(cps (lambda (x) E))	=> (lambda (k) 
;;				     (k (lambda (x) 
;;					  (cps E))))
;;	(cps (E1 E2)		=> (lambda (k) 
;;				     ((cps E1) (lambda (v1) 
;;						 ((cps E2)
;;                                                 (lambda (v2) 
;;						    ((v1 v2) k))))))
;;
;; This should be a pretty straight-forward pmatch-style transformation
(module dumb-cps (cps)
  (import cps-helpers (only lambda-calc-verify verify-lambda-calc))

  ;; The cps procedure performs the entire CPS transformation as
  ;; described above for an overly general CPS transformation
  (define (cps e)
    (begin
      (verify-lambda-calc e)
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
        [else (errorf 'dumb-cps
                "Invalid lambda calculus expression ~s."
                e)])))
)


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
(module olivier-cps (cps)
  (import cps-helpers (only lambda-calc-verify verify-lambda-calc))

  ;; E is the general expression CPSer which takes a standard lambda
  ;; calculus expression, e, and a continuation, k, and returns the
  ;; CPSed version of e using k as the current continuation.
  ;; It accepts the following grammar and performs the associated
  ;; transformations:
  ;;
  ;;    E(x k)                  => (k T(x))
  ;;            where x is any symbol
  ;;
  ;;    E((lambda (x) body) k)  => (k (T (lambda (x) body)))
  ;;
  ;;    E((e0 e1) k)            => (S (e0 e1) k)
  (define (E e k)
    (if (trivial? e)
        `(,k ,(T e))
        (S e k)))

  ;; S is the serious expression CPSer which takes a serious
  ;; expression (i.e. a function application) and a continuation
  ;; variable and returns the CPSed equivalent of the expressions.
  ;; It accepts the following grammar and returns the associated
  ;; transformations:
  ;;
  ;;    (S (t0 t1) k)    => ((t0 t1) k)
  ;;            where expressions t0 and t1 are trivial according to
  ;;            the "trivial?" predicate.
  ;;
  ;;    (S (t0 s1) k)    => (S s1 (lambda (x1) (((T t0) x1) k)))
  ;;            where t0 is trivial according to the "trivial?"
  ;;            predicate, s1 is a serious expression, and x1 is a
  ;;            unique variable.
  ;;
  ;;    (S (s0 t1) k)    => (S s0 (lambda (x0) ((x0 (T t1)) k)))
  ;;            where s0 is a serious expression, t1 is trivial, and
  ;;            x0 is a unique variable
  ;;
  ;;    (S (s0 s1) k)    => (S s0 (lambda (x0)))
  ;;                                (S s1 (lambda (x1)
  ;;                                        ((x0 x1) k)))))
  ;;            where s0 and s1 are serious expressions, and x0 and x1
  ;;            are unique variables
  (define (S e k)      
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
                              ((,x.0 ,x.1) ,k))))))]))

  ;; T is the trivial expression CPSer which takes a trivial lambda
  ;; calculus expression and returns the CPSed equivalent. It accepts
  ;; the following grammar and performs the associated
  ;; transformations:
  ;;
  ;;    (T x)                   => x
  ;;            where x is a symbol
  ;;
  ;;    (T (lambda (x) body))   => (lambda (x)
  ;;                                 (lambda (k) (E body k)))
  ;;            where k is a new (continuation) variable
  (define (T e)
    (pmatch e
      [,x (guard (symbol? x)) x]
      [(lambda (,x) ,body)
       (let ([k (new-var 'k)])
         `(lambda (,x) (lambda (,k) ,(E body k))))]))

  ;; The cps procedure is the driver for the Olivier-style CPS
  ;; tranformation for standard lambda calculus terms. It takes any
  ;; lambda calculus expression and returns the CPSed equivalent
  ;; expression such that it can accept a continuation expression
  ;; (i.e. procedure) and perform the computation according to the
  ;; input continuation.
  (define (cps e)
    (begin
      (verify-lambda-calc e)
      (let ([k (new-var 'k)])
        `(lambda (,k) ,(E e k)))))
)

;; Well, sweet, we have a 50-line Scheme program that will take an
;; arbitrary lambda calculus expression and turn it into an equivalent
;; *CPSed* lambda calculus expression. So where do we go from here? As
;; cool as this is, the lambda calculus is kinda restrictive (we don't
;; even have constants, for glob's sake!), so it would be nice if we
;; could expand out a little bit. In fact, it would be super cool if
;; we could CPS arbitrary *Scheme* programs. But that takes quite a
;; bit of rote extension to defining what's "trivial" and what's
;; "serious," and we don't want to worry with that just yet.
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
;; assumed that they've been properly CPSed.
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
(module n-arity-cps (cps)
  (import cps-helpers (only lambda-calc-verify verify-lambda-calc-na))

  ;; The most elegant way to do this (at least that I've found) is
  ;; using the continuation monad. We include the standard definitions
  ;; of unit (for which we use the Haskell "return" convention) and
  ;; bind.
  (define (returnK e)
    (lambda (k)
      (k e)))

  (define (bindK ma next)
    (lambda (k)
      (let ([k^ (lambda (a)
                  (let ([mb (next a)])
                    (mb k)))])
        (ma k^))))

  ;; We make things even a little better with the letMK and letMK*
  ;; macros (though we mostly include letMK to implement letMK* in
  ;; terms of it). This cleans up the bindKs and makes things look
  ;; akin to Haskells 'do' notation.
  (define-syntax letMK
    (syntax-rules ()
      [(_ ((name init)) expr)
       (bindK init (lambda (name) expr))]))

  (define-syntax letMK*
    (syntax-rules ()
      [(_ ((name init)) expr) 
       (letMK ((name init)) expr)]
      [(_ ((name1 init1) (name2 init2) ...) expr)
       (letMK ([name1 init1]) 
         (letMK* ([name2 init2] ...) expr))]))
  
  ;; E is the general expression CPSer which takes a standard lambda
  ;; calculus expression, e, and a continuation, k, and returns the
  ;; CPSed version of e using k as the current continuation.
  ;; It accepts the following grammar and performs the associated
  ;; transformations:
  ;;
  ;;    E(x k)                  => (k T(x))
  ;;            where x is any symbol
  ;;
  ;;    E((lambda (x) body) k)  => (k (T (lambda (x) body)))
  ;;
  ;;    E((e0 e1) k)            => ((S e) (lambda (s) `(,s ,k)))
  ;;
  ;; Note that the S procedure returns a continuation monad which
  ;; takes an initial continuation and returns a CPSed expression. So
  ;; if the 'e' argument is a serious expression (i.e. function
  ;; application), we pass it to S and apply the resulting monad to a
  ;; continuation that constructs the inner-most application
  ;; expression.
  (define (E e k)
    (if (trivial? e)
        `(,k ,(T e))
        ((S e) (lambda (s) `(,s ,k)))))

  ;; S is the serious expression CPSer which takes a serious
  ;; expression (i.e. a function application) and returns a
  ;; continuation monad.
  ;;
  ;; To properly handle applications for arbitrary arity functions, S
  ;; is recursive and accepts the following grammar with associated
  ;; transformations:
  ;;
  ;; (S '())             => (returnK '())
  ;;
  ;; (S '(t0 r* ...))    => (returnK (cons (T t0) (S r* ...)))
  ;;    where t0 is a trivial expression
  ;;
  ;; (S '(s0 r* ...))    => (bindK (S s0)
  ;;                               (lambda (f)
  ;;                                 (bindK (S r* ...)
  ;;                                        (lambda (r)
  ;;                                          (returnK
  ;;                                            `(,f
  ;;                                               (lambda (s)
  ;;                                                 (cons s r))))))))
  ;;    where s0 is a serious expression and s is a unique variable
  ;;
  ;; Note that with the standard lambda calculus, the continuations we
  ;; passed along to the CPS helpers were the fully-built
  ;; continuations that would appear in the output and returned a
  ;; complete expression. In the case of the arbitrary arity CPS
  ;; transformation, we build the continuation monadically and return
  ;; a monad that takes a continuation for building the innermost
  ;; application expression
  (define (S e)
    (let ([first (or (null? e) (car e))] [rest (or (null? e) (cdr e))])
      (cond
        [(null? e) (returnK '())]
        [(trivial? first)
         (let ([fst (T first)])
           (letMK ([restMK (S rest)])
             (returnK (cons fst restMK))))]
        [else
          (let ([s (new-var 's)])
            (letMK* ([fstMK (S first)]
                     [restMK (S rest)]
                     [inner-call (cons s restMK)])
              (returnK `(,fstMK (lambda (,s) ,inner-call)))))])))


  ;; T is the trivial expression CPSer which takes a trivial lambda
  ;; calculus expression and returns the PSed equivalent. It accepts
  ;; the following grammar and performs the associated
  ;; transformations:
  ;;
  ;;    (T x)                   => x
  ;;            where x is a symbol
  ;;
  ;;    (T (lambda (x) body))   => (lambda (x)
  ;;                                 (lambda (k) (E body k)))
  ;;            where k is a new (continuation) variable
  ;;
  ;; Note that this is functionally the same as the T helper used in
  ;; the olivier-cps module, since nothing actually needs to be done
  ;; with the list of formals in the lambda case and trivial
  ;; expressions are otherwise exactly the same as before.
  (define (T e)
    (pmatch e
      [,x (guard (symbol? x)) x]
      [(lambda ,fmls ,body) (let ([k (new-var 'k)])
                              `(lambda ,fmls
                                 (lambda (,k) ,(E body k))))]))

  ;; The cps procedure is the driver for the Olivier-style CPS
  ;; tranformer for lambda calculus expressions with procedures of
  ;; arbitrary arity.
  (define (cps e)
    (begin
      (verify-lambda-calc-na e)
      (let ([k (new-var 'k)])
        `(lambda (,k) ,(E e k)))))
)
