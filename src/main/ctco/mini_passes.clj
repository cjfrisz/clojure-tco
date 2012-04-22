;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified 22 Apr 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-flag
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns ctco.mini-passes
  (:require [ctco.expr
             app atomic fn defn do if let loop recur simple-op]
            [ctco.util
             [new-var :as nv]])
  (:import [ctco.expr.app
            App]
           [ctco.expr.atomic
            Atomic]
           [ctco.expr.fn
            Fn]
           [ctco.expr.defn
            Defn]
           [ctco.expr.do
            DoSync]
           [ctco.expr.if
            IfCps]
           [ctco.expr.let
            Let]
           [ctco.expr.loop
            Loop]
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
  trampoline function named by the 'tramp' argument, passing 'flag' as the
  reference for when the computation is finished.

  The version of the function that takes n+1 arguments takes a continuation as
  the (n+1)st argument and does the actual computational heavy lifting.

  If the input expression doesn't represent a function type then the expression
  is simply returned."
  [expr tramp flag]
  (if (instance? Defn expr)
      (let [fml* (:fml* (first (:func* expr)))
            fml-bl* (vec (butlast fml*))
            rand* (conj fml-bl* flag)
            init-call (App. (:name expr) rand*)
            tramp-call (App. tramp [init-call flag])
            func* (vec (cons (Fn. fml-bl* tramp-call) (:func* expr)))]
        (Defn. (:name expr) func*))
      expr))

(defn make-flag
  "Initializes the flag value for expr with the name given by flag by
  introducing it through a 'let' binding.

  At current, the flag is a ref initialized to 'false.'"
  [expr flag]
  (let [init (SimpleOpCps. 'ref [(Atomic. 'false)])
        bind* [flag init]]
    (Let. bind* expr)))

(defn make-apply-k
  "Introduces the definition of the continuation application function for expr
  using apply-k as the name for the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr apply-k]
  (let [kont (nv/new-var 'k)
        arg (nv/new-var 'a)]
    (let [test (SimpleOpCps. 'fn? [kont])
          conseq (App. kont [arg])
          alt (DoSync. [(SimpleOpCps. 'ref-set [kont (Atomic. 'true)]) arg])
          body (IfCps. test conseq alt)
          init (Fn. [kont arg] body)
          bind* [apply-k init]]
      (Let. bind* expr))))

(defn make-trampoline
  "Introduces the definition of the trampoline function for expr using tramp as
  the name of the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr tramp]
  (let [thunk (nv/new-var 'thunk)
        flag (nv/new-var 'flag)
        test (SimpleOpCps. 'deref [flag])
        conseq (DoSync. [(SimpleOpCps. 'ref-set [flag (Atomic. 'false)]) thunk])
        alt (Recur. [(App. thunk [])])
        loop-body (IfCps. test conseq alt)
        body (Loop. [thunk thunk] loop-body)
        init (Fn. [thunk flag] body)]
    (Let. [tramp init] expr)))
