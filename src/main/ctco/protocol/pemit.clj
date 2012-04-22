;;----------------------------------------------------------------------
;; File pemit.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 22 Apr 2012
;; 
;; Defines the protocol for emitting a sequence representing the
;; Clojure syntax of a record.
;;----------------------------------------------------------------------

(ns ctco.protocol.pemit)

(defprotocol PEmit
  "Protocol for TCO expressions that can be represented as a sequence."
  (emit [this]
    "Emits a sequence representing the Clojure syntax for the TCO expression."))
