;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Atomic record type for atomic expressions (e.g.
;; numbers, booleans, symbols, etc.) in the CTCO compiler.
;;
;; It implements the following records:
;;
;;      PAbstractK:
;;              Simply returns the Atomic.
;;
;;      PCpsTriv:
;;              Simply returns the Atomic.
;;
;;      PEmit:
;;              Returns the syntax for the Atomic.
;;
;;      PThunkify:
;;              Simply returns the Atomic.
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
