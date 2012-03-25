;;----------------------------------------------------------------------
;; File tco.clj
;; Written by Chris Frisz
;; 
;; Created  5 Mar 2012
;; Last modified 25 Mar 2012
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
  (:use [clojure-tco.util
         :only (new-var reset-var-num)]))

(defn- define-tramp
  "Given an expression and a name, returns the expression wrapped in a letfn
  defining a trampoline function with the given name."
  [expr name]
  (let [thunk (new-var 'thunk)
        done (new-var 'done)]
    `(~'letfn [(~name [~thunk ~done]
               (~'loop [~thunk ~thunk]
                 (~'if (~'true? @~done)
                     (~'do (~'dosync (~'ref-set ~done false)) ~thunk)
                     (~'recur (~thunk)))))]
       ~expr)))

(defn- define-apply-k
  "Given an expression and a name, returns the expression wrapped in a letfn
  defining a continuation application function with the given name."
  [expr name]
  (let [kont (new-var 'k)
        arg (new-var 'a)
        done (new-var 'done)]
    `(~'letfn [(~name [~kont ~arg]
               (~'if (~'and (~'seq? ~kont) (~'= (~'first ~kont) 'empty-k))
                   (~'let [~done (~'first (~'rest ~kont))]
                     (~'do (~'dosync (~'ref-set ~done true)) ~arg))
                   (~kont ~arg)))]
       ~expr)))

(defn- define-done
  "Given an expression and a name, returns the expression wrapped in a let
  introducing a reference used as a done flag."
  [expr name]
  `(~'let [~name (~'ref false)]
     ~expr))

(defn- overload
  "Given an expression and a symbol representing the name of a done variable,
  overloads the function so it can be called with or without a continuation
  argument. Also sets up the version called without the continuation argument
  to create a proper empty continuation and call off to the CPS version."
  [expr tramp done]
  (let [thunk (new-var 'thunk)]
    (match [expr]
      [(['defn name fml* body] :seq)] (let [fml-bl* (butlast fml*)]
                                        `(~'defn ~name
                                           ([~@fml-bl*]
                                              (~'let [~thunk (~name
                                                              ~@fml-bl*
                                                              (~'list
                                                               'empty-k
                                                               ~done))]
                                                (~tramp ~thunk ~done)))
                                           (~fml* ~body)))
      :else expr)))

(defn tco
  "Takes a sequence representing a Clojure expression and returns a
  sequence representing the original CPSed and trampolined to allow
  for constant-space tail calls."
  [expr]
  (do 
    (reset-var-num)
    ;; Variables
    (let [tramp (new-var 'tramp)
          apply-k (new-var 'apply-k)
          done (new-var 'done)]
      ;; Code transformations
      (let [expr (cps expr)
            expr (abstract-k expr apply-k)
            expr (thunkify expr)
            expr (overload expr tramp done)]
        ;; Bindings
        (let [expr (define-done expr done)
              expr (define-tramp expr tramp)
              expr (define-apply-k expr apply-k)]
          expr)))))
