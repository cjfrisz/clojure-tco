;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 22 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns ctco.expr.atomic
  (:require [ctco.protocol
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
