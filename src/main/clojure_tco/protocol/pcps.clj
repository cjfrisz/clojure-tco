;;----------------------------------------------------------------------
;; File pcps.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  2 Apr 2012
;; 
;; Defines the PCps protocol for TCO expressions that can be CPSed.
;;----------------------------------------------------------------------

(ns clojure-tco.protocol.pcps)

(defprotocol PCps
  "Protocol for expressions that be transformed to CPS."
  (triv? [this]
    "Returns whether the expression is trivial with respect to the Olivier-style
    CPS algorithm.")
  (cps [this] [this k]
    "Specifies the CPS transformation for a given expression."))
