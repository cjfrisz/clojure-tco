;;----------------------------------------------------------------------
;; File pabstract_k.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the protocol for Clojure TCO expressions for which
;; continuation application can be abstracted.
;;----------------------------------------------------------------------

(ns bbc.protocol.pabstract-k)

(defprotocol PAbstractK
  "Defines the 'abstract-k' function which abstracts over continutation
  application using the given symbol to represent the function that implements
  continutation appliction."
  (abstract-k [this app-k]
    "Abstracts over continutation application in an expression by converting
    continuation applications to calls to app-k."))
