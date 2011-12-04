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

(define var-num (make-parameter 0))

(define cps
  (lambda (e)
    (define new-var
      (lambda (sym)
	(let ([new-var (string->symbol 
			(string-append (symbol->string sym) 
				       "."
				       (number->string (var-num))))])
	  (begin
	    (var-num (add1 (var-num)))
	    new-var))))
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
      [else (error 'cps "Invalid lambda calculus expression ~s." e)])))

		    