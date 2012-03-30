;;----------------------------------------------------------------------
;; File expr.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 30 Mar 2012
;; 
;; Defines the base protocol for all expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr)

(defprotocol PExpr
  "Protocol for expressions in the TCO compiler, including all the operations
  and transformations required."
  (cps [this]
    "CPS transformation for an expression.")
  (abstract-k [this app-k]
    "Abstract over applying continuations using app-k.")
  (thunkify [this]
    "Transform all functions to return thunks."))
