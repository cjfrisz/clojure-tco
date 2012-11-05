;;----------------------------------------------------------------------
;; File simple.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 20 Oct 2012
;; 
;; Defines the Simple record type for simple expressions (e.g.
;; numbers, booleans, symbols, etc.) in the CTCO compiler.
;;
;; It implements the following records:
;;
;;      PCpsTriv:
;;              Simply returns the Simple.
;;
;;      PLoadTrampoline:
;;              Simply returns the Simple.
;;
;;      PThunkify:
;;              Simply returns the Simple.
;;
;;      PUnparse:
;;              Returns the syntax for the Simple.
;;
;;      PUnRecurify:
;;              Simply returns the Simple.
;;----------------------------------------------------------------------

(ns ctco.expr.simple
  (:require [ctco.protocol :as proto]))

(defrecord Simple [val]
  proto/PCpsTriv
  (cps-triv [this] this)

  proto/PLoadTrampoline
  (load-tramp [this _] this)

  proto/PRecurify
  (recurify [this _ _ _] this)

  proto/PThunkify
  (thunkify [this] this)

  proto/PUnparse
  (unparse [this] (:val this))

  proto/PUnRecurify
  (unrecurify [this _] this))
