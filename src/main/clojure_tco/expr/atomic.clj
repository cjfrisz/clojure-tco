;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 11 Apr 2012
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

(defrecord Num [val])

(defrecord Bool [val])

(defrecord Var [val])

(defrecord Sym [val])

(def atomic-abs-k
  {:abstract-k (fn [this app-k] this)})

(def atomic-cps-triv
  {:cps identity})

(def atomic-emit
  {:emit (fn [this] (quote (:val expr)))})

(def atomic-thunkify
  {:thunkify identity})

(extend Bool
  pabs-k/PAbstractK
    atomic-abs-k
  
  triv/PCpsTriv
    atomic-cps-triv

  pemit/PEmit
    atomic-emit

  pthunkify/PThunkify
    atomic-thunkify)

(extend Num
  pabs-k/PAbstractK
    atomic-abs-k

  triv/PCpsTriv
    atomic-cps-triv

  pemit/PEmit
    atomic-emit

  pthunkify/PThunkify
    atomic-thunkify)

(extend Sym
  pabs-k/PAbstractK
    atomic-abs-k

  triv/PCpsTriv
    atomic-cps-triv

  pemit/PEmit
    atomic-emit

  pthunkify/PThunkify
    atomic-thunkify)

(extend Var
  pabs-k/PAbstractK
    atomic-abs-k

  triv/PCpsTriv
    atomic-cps-triv

  pemit/PEmit
    atomic-emit

  pthunkify/PThunkify
    atomic-thunkify)
