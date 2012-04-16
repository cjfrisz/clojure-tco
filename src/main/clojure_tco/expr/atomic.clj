;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 16 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.expr.atomic
  (:require [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pcps-triv :as triv]
             [pemit :as pemit]
             [pthunkify :as pthunkify]]))

(defrecord Atomic [val]
  pabs-k/PAbstractK
    (abstract-k [this _] this)

  triv/PCpsTriv
    (cps [this] this)

  pemit/PEmit
    (emit [this] (:val this))

  pthunkify/PThunkify
    (thunkify [this] this))

(defn make-atomic
  "Takes the value for an atomic and returns an Atomic record type with that
  value."
  [val]
  (Atomic. val))
