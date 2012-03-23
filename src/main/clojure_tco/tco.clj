;;----------------------------------------------------------------------
;; File tco.clj
;; Written by Chris Frisz
;; 
;; Created  5 Mar 2012
;; Last modified 22 Mar 2012
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
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.cps
         :only (cps)])
  (:use [clojure-tco.abstract-k
         :only (abstract-k)])
  (:use [clojure-tco.thunkify
         :only (thunkify)])
  (:use [clojure-tco.tramp
         :only (tramp)])
  (:use [clojure-tco.util
         :only (new-var reset-var-num)]))

(defn- overload
  "Takes a sequence representing a CPSed Clojure expression and, if it
  is a function definition, overloads it so that it can interoperate
  with existing code."
  [expr]
  (match [expr]
    [(['defn name fml* & body*] :seq)] (let [fml-bl* (butlast fml*)]
                                         `(~'defn ~name
                                            (~fml-bl*
                                             (~name ~@fml-bl* '(empty-k)))
                                            (~fml*
                                             ~@body*)))
    :else expr))

(defn tco
  "Takes a sequence representing a Clojure expression and returns a
  sequence representing the original CPSed and trampolined to allow
  for constant-space tail calls."
  [expr]
  (do 
    (reset-var-num)
    (let [tramp-fn (new-var 'tramp)
          thv (new-var 'th)
          donev (new-var 'done)
          apply-k (new-var 'apply-k)]
      (let [expr-cps (cps expr)
            expr-cps-absk (abstract-k expr-cps apply-k)
            expr-cps-absk-th (thunkify expr-cps-absk)
            expr-tco (tramp expr-cps-absk-th tramp-fn)
            expr-tco-ol (overload expr-tco)]
        `(~'letfn [(~tramp-fn [~thv ~donev]
                     (~'loop [~thv ~thv]
                       (~'if @~donev ~thv (~'recur (~thv)))))
                   (~apply-k [k# a#]
                     (~'if (~'= k# '(empty-k))
                           (~'do (~'dosync (~'ref-set ~donev ~'true)) a#)
                           (k# a#)))]
           ~expr-tco-ol)))))
