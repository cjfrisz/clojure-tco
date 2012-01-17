;;----------------------------------------------------------------------
;; File clojure-cps.ss
;; Written by Chris Frisz
;; 
;; Created  6 Jan 2012
;; Last modified 17 Jan 2012
;; 
;; The file clojure-cps.ss provides utilities for CPSing Clojure
;; programs.
;;----------------------------------------------------------------------

(library (clojure-cps)

  (export clj-cps)

  ;; Library import
  (import (chezscheme)
          (match)
          (lib util)
          (lib monad))

  ;; Module import
  (import monadK)

(define (trivial? t)
  (match t
    [true #t]
    [false #t]
    [,s (guard (symbol? s)) #t]
    [,n (guard (number? n)) #t]
    [(fn ,fmls* ,body) #t]
    [,else #f]))

(define (E e k)
  (if (trivial? e)
      `(,k ,(T e))
      ((S e) (lambda (s) `(,s ,k)))))

(define (S e)
  (if (null? e)
      (returnK '())
      (let ([fst (car e)] [rst (cdr e)])
        (match fst
          [,t
            (guard (trivial? t))
           (let ([fst (T fst)])
             (letMK ([rstMK (S rst)])
               (returnK (cons fst rstMK))))]
          [(,rator ,rand)
            (let ([s (new-var 's)])
              (letMK* ([fstMK (S fst)] [rstMK (S rst)]
                       [inner-call (cons s rstMK)])
                (returnK `(,fstMK (fn [,s] ,inner-call)))))]))))

(define (T e)
  (match e
    [,x (guard (or (number? x) (symbol? x))) x]
    [(fn ,fmls* ,body)
     (let ([k (new-var 'k)])
       `(fn ,fmls* (fn [,k] ,(E body k))))]))

(define (clj-cps e)
  (begin
    (reset-var-num)
    (let ([k (new-var 'k)])
      `(fn [,k] ,(E e k)))))

(pretty-format 'fn '(fn [bracket var x 0 ...] e))

)

