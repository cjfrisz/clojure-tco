;;----------------------------------------------------------------------
;; File pwalkable.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the base protocol for all expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.protocol.pwalkable)

(defprotocol PWalkable
  "Protocol for TCO expressions that can be walked."
  (walk-expr [this f ctor] [this f ctor args]
    "Applies the function f to the subforms of the argument expression,
    returning an expression created with the constructor."))
