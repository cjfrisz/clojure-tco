;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns ctco.expr.atomic
  (:require [ctco.protocol :as proto]))

(defrecord Atomic [val]
  proto/PAbstractK
    (abstract-k [this _] this)

  proto/PCpsTriv
    (cps-triv [this] this)

  proto/PEmit
    (emit [this] (:val this))

  proto/PThunkify
    (thunkify [this] this))
