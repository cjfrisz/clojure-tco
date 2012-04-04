;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  4 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.expr.atomic
  (:require [clojure-tco.protocol
             [pcps :as pcps] 
             [pthunkify :as pthunkify]]))

(defrecord Num [val])

(defrecord Bool [val])

(defrecord Var [val])

(defrecord Sym [val])

(def atomic-cps
  {:triv? (fn [_] true)
   :cps (fn ([this] this) ([this _] this))})

(def atomic-thunkify
  {:thunkify identity})

(extend Num
  pcps/PCps
  atomic-cps

  pthunkify/PThunkify
  atomic-thunkify)

(extend Bool
  pcps/PCps
  atomic-cps

  pthunkify/PThunkify
  atomic-thunkify)

(extend Var
  pcps/PCps
  atomic-cps

  pthunkify/PThunkify
  atomic-thunkify)

(extend Sym
  pcps/PCps
  atomic-cps

  pthunkify/PThunkify
  atomic-thunkify)
