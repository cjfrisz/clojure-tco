;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified 29 Aug 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns ctco.mini-passes
  (:use [clojure.core.match
         :only (match)]))

;; NB: This only overloads one of the variadic forms for a "defn" expression.
;; NB: Need to iterate over all forms to properly transform the whole
;; NB: definition. 
(defn overload
  "Takes an expression in the TCO compiler (in record representation) and, if it
  represents a function type (i.e. 'defn'), overloads the expression.

  That is, for a function that takes n arguments, it is overloaded such that it
  can take n and n+1 arguments. The version that takes n arguments corresponds
  to the version that outside functions call into. It sets up the computation to
  be run on the trampoline introduced by the TCO compiler by initializing the
  empty continuation and calling the version that takes n+1 arguments. The
  result of the initial call is then loaded onto the trampoline by calling the
  trampoline function named by the 'tramp' argument, passing an additional nil
  argument to call the overloaded arity.

  The version of the function that takes n+1 arguments takes a continuation as
  the (n+1)st argument and does the actual computational heavy lifting.

  If the input expression doesn't represent a function type then the expression
  is simply returned."
  [expr tramp]
  ;; NB: You will notice that this is horrible code that no man in his right
  ;; NB: mind should have written. First, this is transitional code before
  ;; NB: getting rid of overloading for defn expressions entirely. Second, why
  ;; NB: would you say things like that about me?
  (match [expr]
    [(['clojure.core/defn name ([fml* & bexpr*] :seq)] :seq)]
      (let [fml-bl* (vec (butlast fml*))]
        `(defn ~name
           (~fml-bl* (~tramp (~name ~@fml-bl* nil)))
           (~fml* ~@bexpr*)))
    :else expr))

(defn make-apply-k
  "Introduces the definition of the continuation application function for expr
  using apply-k as the name for the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr apply-k]
  (let [kont (gensym 'kont)
        arg (gensym 'arg)]
    `(letfn [(~apply-k [~kont ~arg]
               (if (fn? ~kont)
                   (~kont ~arg)
                   ~arg))]
       ~expr)))

(defn make-trampoline
  "Introduces the definition of the trampoline function for expr using tramp as
  the name of the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr tramp]
  (let [thunk (gensym 'thunk)]
    `(letfn [(~tramp [~thunk]
               (if (get (meta ~thunk) :thunk)
                   (recur (~thunk))
                   ~thunk))]
       ~expr)))
