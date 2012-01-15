;;----------------------------------------------------------------------
;; File monad.ss
;; Written by Chris Frisz
;; 
;; Created 15 Jan 2012
;; Last modified 15 Jan 2012
;; 
;; Defines a library (lib monad) with monads for doing CPS.
;;----------------------------------------------------------------------

(library
  (lib monad)

  (export monadK)

  (import (rnrs) (only (chezscheme) module))

(module monadK (returnK bindK letMK letMK*)

(define (returnK e)
  (lambda (k)
    (k e)))

(define (bindK ma next)
  (lambda (k)
    (let ([k^ (lambda (a)
                (let ([mb (next a)])
                  (mb k)))])
      (ma k^))))

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

) ;; End of module monadK

) ;; End of library (lib monad)
