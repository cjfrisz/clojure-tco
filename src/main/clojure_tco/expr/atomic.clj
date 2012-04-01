;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  1 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.atomic
  (:require [clojure-tco.expr :as expr]))

(def atomic-base
  {:triv?       (fn [this] true)
   :walk-expr   (fn [this & _] this)
   :cps         (fn [this & _] this)
   :abstract-k  (fn [this _] this)
   :thunkify    identity})

(defrecord Boolean [val])

(extend Boolean
  expr/PExpr
  atomic-tco-fns)

(defrecord Number [val])

(extend Number
  expr/PExpr
  atomic-tco-fns)

(defrecord Symbol [val])

(extend Symbol
  expr/PExpr
  atomic-tco-fns)
