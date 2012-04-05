;;----------------------------------------------------------------------
;; File pwalkable.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the protocol for TCO expressions which can be walked
;;----------------------------------------------------------------------

(ns clojure-tco.protocol.pwalkable)

(defprotocol PWalkable
  "Protocol for TCO expressions that can be walked."
  (walk-expr [this f ctor]
    "Applies the function f to the subforms of the argument expression,
    returning an expression created with the constructor."))
