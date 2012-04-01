;;----------------------------------------------------------------------
;; File pexpr.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  1 Apr 2012
;; 
;; Defines the base protocol for all expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.pexpr)

(defprotocol PExpr
  "Protocol for expressions in the TCO compiler, including all the operations
  and transformations required."
  (triv? [this]
    "Returns whether the expression is trivial with respect to the Olivier-style
    CPS algorithm.")
  (walk-expr [this f & args]
    "Applies the function (with optional arguments) to the expression subforms
    of the expression.")
  (cps [this & k]
    "CPS transformation for an expression.")
  (thunkify [this]
    "Transform all functions to return thunks."))
