;;----------------------------------------------------------------------
;; File cps-tester.ss
;; Written by Chris Frisz
;; 
;; Created 20 Jan 2012
;; Last modified 21 Jan 2012
;; 
;; Defines a set of tests and a simple test macro for the 
;;----------------------------------------------------------------------

(import (lambda-calc-cps))

;; Only care about n-arity-cps for now
(import n-arity-cps)

(define-syntax cps-test
  (syntax-rules (k style)
    [(_ name e (k k-init) [var init] ...)
     (let ([expr (cps e)]
           [bind* `([var init] ...)])
       (let ([expr^ `(let ([var init] ...) ,expr)])
         (printf "Testing ~s -- " name)
         (let ([evaluated-expr (eval `(,expr^ ,k-init))])
           (printf "Passed!~%")
           (printf "  Original expression: ~s~%" e)
           (printf "  CPSed expression: ~s~%~%" expr)
           (when (not (null? bind*))
             (printf "  Bindings used:~%")
             (for-each (lambda (b)
                         (printf "    ~s: ~s~%" (car b) (cadr b)))
                       bind*)
             (printf "~%"))
           (printf "  Initial continuation: ~s~%~%" k-init)
           (printf "  Output: ~s~%~%" evaluated-expr))))]
    [(_ name e [var init] ...)
     (cps-test name e (k '(lambda (x) x)) [var init] ...)]))

(cps-test 'id '(lambda (x) x))

(cps-test 'simple-app '((lambda (x) x) y) [y 5])

(cps-test 'K '(lambda (x) (lambda (y) x)))

(cps-test 'K-app '(((lambda (x) (lambda (y) x)) a) b) [a 2] [b 5])

(cps-test 'add1-k 'a (k '(lambda (x) (add1 x))) [a 3])

(cps-test 'church-num1 '(lambda (f) (lambda (x) (f x))))

(cps-test 'lotso-redex
  '(((lambda (x) x) (lambda (y) y)) (((lambda (u) (lambda (v) u)) a) b))
  [a 3]
  [b 12])

(cps-test 'lotso-redex2
  '(((lambda (x) x) (lambda (y) y)) a)
  [a 3])

(cps-test 'id-on-id
  '((lambda (x) x) (lambda (y) y)))
