;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  2 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.expr.atomic
  (:require [clojure-tco.expr.pexpr :as pexpr]))

(def atomic-base
  {:triv?       (fn [this] true)
   :walk-expr   (fn [this & _] this)
   :cps         (fn [this & _] this)
   :thunkify    identity})

(defrecord Bool [val])

(extend Bool
  pexpr/PExpr
  atomic-base)

(defrecord Num [val])

(extend Num
  pexpr/PExpr
  atomic-base)

(defrecord Sym [val])

(extend Sym
  pexpr/PExpr
  atomic-base)
