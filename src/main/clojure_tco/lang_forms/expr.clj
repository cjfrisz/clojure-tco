;;----------------------------------------------------------------------
;; File expr.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 31 Mar 2012
;; 
;; Defines the base protocol for all expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr)

(defprotocol PExpr
  "Protocol for expressions in the TCO compiler, including all the operations
  and transformations required."
  (walk-expr [this f & args]
    "Applies the function (with optional arguments) to the expression subforms
    of the expression.")
  (cps [this & k]
    "CPS transformation for an expression.")
  (abstract-k [this app-k]
    "Abstract over applying continuations using app-k.")
  (thunkify [this]
    "Transform all functions to return thunks."))
