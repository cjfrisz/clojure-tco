;;----------------------------------------------------------------------
;; File clojure-cps.ss
;; Written by Chris Frisz
;; 
;; Created  6 Jan 2012
;; Last modified  3 Feb 2012
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
                 (let ([k^ `(fn [,s] ,rst^)])
                   (S fst k^)))))))))

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

