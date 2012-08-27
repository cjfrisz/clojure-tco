;;----------------------------------------------------------------------
;; File simple.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Aug 2012
;; 
;; Defines the Simple record type for simple expressions (e.g.
;; numbers, booleans, symbols, etc.) in the CTCO compiler.
;;
;; It implements the following records:
;;
;;      PAbstractK:
;;              Simply returns the Simple.
;;
;;      PCpsTriv:
;;              Simply returns the Simple.
;;
;;      PEmit:
;;              Returns the syntax for the Simple.
;;
;;      PThunkify:
;;              Simply returns the Simple.
;;----------------------------------------------------------------------

(ns ctco.expr.simple
  (:require [ctco.protocol :as proto]))

(defrecord Simple [val]
  proto/PAbstractK
    (abstract-k [this _] this)

  proto/PCpsTriv
    (cps-triv [this] this)

  proto/PEmit
    (emit [this] (:val this))

  proto/PThunkify
    (thunkify [this] this))
