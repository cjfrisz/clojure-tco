;;----------------------------------------------------------------------
;; File lambda-calc-cps.ss
;; Written by Chris Frisz
;; 
;; Created 3 Dec 2011
;; Last modified  3 Feb 2012
;; 
;; The file lambda-calc-cps.ss defines the lambda-calc-cps library
;; which contains several modules for CPSing lambda calculus
;; expressions. They are as follows:
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

(library (n-arity-cps)

  (export cps)

  (import (chezscheme))

  ;; This is gonna need pmatch
  (include "pmatch.scm")

;; Use an essentially global counter to ensure that each variable is
;; unique.
(define var-num (make-parameter 0))

(define (reset-var-num)
  (var-num 0))

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


;; The verify-lambda-calc-na procedure verifies a lambda calculus
;; expression where procedures may take an arbitrary number of
;; arguments. 
(define (verify-lambda-calc-na e)
  (pmatch e
    [,x (guard (symbol? x)) #t]
    [(lambda (,fst . ,rst) ,body) (verify-lambda-calc-na body)]
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
                 e)]))  ;; End of lambda-calc-verify

(define (E e k)
  (if (trivial? e)
      `(,k ,(T e))
      (S e k)))

(define (S e k)
 (let loop ([e e] [k k] [call '()])
   (if (null? e)
       `(,@call ,k)
       (let ([fst (car e)] [rst (cdr e)])
         (if (trivial? fst)
             (let ([fst^ (T fst)])
               (loop rst k `(,@call ,fst^)))
             (let ([s (new-var 's)])
               (let ([rst^ (loop rst k `(,@call ,s))])
                 (let ([k^ `(lambda (,s) ,rst^)])
                   (S fst k^)))))))))

(define (T e)
  (pmatch e
    [,x (guard (symbol? x)) x]
    [(lambda ,fmls ,body)
     (let ([k (new-var 'k)])
       (let ([fmls^ (append fmls `(,k))])
       `(lambda ,fmls^ ,(E body k))))]))

(define (cps e)
  (begin
    (verify-lambda-calc-na e)
    (reset-var-num)
    (let ([k (new-var 'k)])
      `(lambda (,k) ,(E e k)))))

)  ;; End of library n-arity-cps
