;;----------------------------------------------------------------------
;; File tco.clj
;; Written by Chris Frisz
;; 
;; Created  5 Mar 2012
;; Last modified  5 Mar 2012
;; 
;; Defines the "tco" function, which takes a sequence representing a
;; Clojure expression and returns the expression CPSed and
;; trampolined to allow for proper tail calls for functions defined in
;; terms of recursive function calls. This provides constant-space
;; tail calls for self-recursion and arbitrary mutual recursion.
;;
;; This is intended to improve on Clojure's built-in support for
;; constant-space self-recursion (via the "recur" form in a function's
;; tail position) and two-function mutual recursion (via the
;; "trampoline" function).
;;----------------------------------------------------------------------

(ns clojure-tco.tco
  (:use [clojure-tco.cps
         :only (cps)])
  (:use [clojure-tco.tramp
         :only (thunkify tramp)])
  (:use [clojure-tco.util
         :only (new-var reset-var-num)]))

(defn tco
  "Takes a sequence representing a Clojure expression and returns a
  sequence representing the original CPSed and trampolined to allow
  for constant-space tail calls."
  [expr]
  (let [expr-cps (cps expr)
        expr-cps-th (thunkify expr-cps)
        tramp-fn (new-var 'tramp)
        expr-tco (tramp expr-cps-th tramp-fn)
        thv (new-var 'th)
        donev (new-var 'done)]
    `(~'letfn [(~tramp-fn [~thv ~donev]
                 (~'loop [~thv ~thv]
                   (~'if @~donev ~thv (~'recur (~thv)))))]
       ~expr-tco)))
