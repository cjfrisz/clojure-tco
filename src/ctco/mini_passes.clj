;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified 26 Aug 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns ctco.mini-passes
  (:require [ctco.expr
             app simple fn defn do if let recur simple-op]
            [ctco.util :as util])
  (:import [ctco.expr.app
            App]
           [ctco.expr.simple
            Simple]
           [ctco.expr.fn
            FnBody]
           [ctco.expr.defn
            Defn]
           [ctco.expr.do
            Do]
           [ctco.expr.if
            IfCps]
           [ctco.expr.let
            LetCps]
           [ctco.expr.recur
            Recur]
           [ctco.expr.simple_op
            SimpleOpCps]))

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
  (if (instance? Defn expr)
      (let [fml* (:fml* (first (:func* expr)))
            fml-bl* (vec (butlast fml*))
            rand* (conj fml-bl* (Simple. 'nil))
            init-call (App. (:name expr) rand*)
            tramp-call (App. tramp [init-call])
            func* (vec (cons (FnBody. fml-bl* tramp-call) (:func* expr)))]
        (Defn. (:name expr) func*))
      expr))

(defn make-apply-k
  "Introduces the definition of the continuation application function for expr
  using apply-k as the name for the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr apply-k]
  (let [kont (util/new-var 'k)
        arg (util/new-var 'a)]
    (let [test (SimpleOpCps. 'fn? [kont])
          conseq (App. kont [arg])
          body (IfCps. test conseq arg)
          init (FnBody. [kont arg] body)
          bind* [apply-k init]]
      (LetCps. bind* expr))))

(defn make-trampoline
  "Introduces the definition of the trampoline function for expr using tramp as
  the name of the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr tramp]
  (let [thunk (util/new-var 'thunk)
        test (SimpleOpCps. 'get [(SimpleOpCps. 'meta [thunk]) (Simple. :thunk)])
        conseq (Recur. [(App. thunk [])])
        body (IfCps. test conseq thunk)
        init (FnBody. [thunk] body)]
    (LetCps. [tramp init] expr)))
