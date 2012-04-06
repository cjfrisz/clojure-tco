;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  5 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.expr.atomic
  (:require [clojure-tco.protocol
             [pcps-triv :as triv] 
             [pthunkify :as pthunkify]]))

(defrecord Num [val])

(defrecord Bool [val])

(defrecord Var [val])

(defrecord Sym [val])

(def atomic-cps-triv
  {:cps identity})

(def atomic-thunkify
  {:thunkify identity})

(extend Num
  triv/PCpsTriv
    atomic-cps-triv

  pthunkify/PThunkify
    atomic-thunkify)

(extend Bool
  triv/PCpsTriv
    atomic-cps-triv

  pthunkify/PThunkify
    atomic-thunkify)

(extend Var
  triv/PCpsTriv
    atomic-cps-triv

  pthunkify/PThunkify
    atomic-thunkify)

(extend Sym
  triv/PCpsTriv
    atomic-cps-triv

  pthunkify/PThunkify
    atomic-thunkify)
