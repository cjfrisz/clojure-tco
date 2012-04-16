;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified 15 Apr 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-flag
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns clojure-tco.mini-passes
  (:require [clojure-tco.expr
             [app defn]])
  (:import [clojure_tco.expr.defn
            Defn]))

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
  (if (instance? Defn. expr)
      (let [fml* (first (:func* expr))
            fml-bl* (vec (butlast fml*))
            rand* (conj fml-bl* done)
            init-call (App. (:name expr) rand*)
            tramp-call (App. tramp [flag])
            body (App. tramp-call [init-call])
            func* (concat [fml-bl* body] (:func* expr))]
        (Defn. (:name expr) func*))))
