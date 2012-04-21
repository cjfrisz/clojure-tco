;;----------------------------------------------------------------------
;; File pwalkable.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the protocol for TCO expressions which can be walked
;;----------------------------------------------------------------------

(ns bbc.protocol.pwalkable)

(defprotocol PWalkable
  "Protocol for TCO expressions that can be walked."
  (walk-expr [this f ctor]
    "Applies the function f to the subforms of the argument expression,
    returning an expression created with the constructor."))
