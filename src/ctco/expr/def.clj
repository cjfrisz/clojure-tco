;;----------------------------------------------------------------------
;; File def.clj
;; Written by Chris
;; 
;; Created 30 Aug 2012
;; Last modified  6 Sep 2012
;; 
;; Defines the DefSrs, DefTriv, and DefCps record types for representing
;; 'def' expression in the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.def
  (:require [ctco.protocol :as proto])
  (:use [ctco.util :only (extend-group)]))

(defrecord DefCps [sym init]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) #(DefCps. %1 %2)))
  
  proto/PAlphaRename
    (alpha-rename [this old new]
      (proto/walk-expr this #(proto/alpha-rename % old new) #(DefCps. %1 %2)))
  
  proto/PThunkify
    (thunkify [this]
      (proto/walk-expr this proto/thunkify #(DefCps. %1 %2))))

(defrecord DefSrs [sym init]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) #(DefSrs. %1 %2)))

  proto/PAlphaRename
    (alpha-rename [this old new]
      (proto/walk-expr this #(proto/alpha-rename % old new) #(DefSrs. %1 %2)))

  proto/PCpsSrs
    (cps-srs [this k]
      (proto/walk-expr this #(proto/cps-srs % k) #(DefCps. %1 %2))))

(defrecord DefTriv [sym init]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) #(DefTriv. %1 %2)))

  proto/PAlphaRename
    (alpha-rename [this old new]
      (proto/walk-expr this #(proto/alpha-rename % old new) #(DefTriv. %1 %2)))

  proto/PCpsTriv
    (cps-triv [this]
      (proto/walk-expr this proto/cps-triv #(DefCps. %1 %2))))

(def def-gather-free-vars
  {:gather-free-vars (fn [this]
                       (cons (:sym this) 
                         (proto/gather-free-vars (:init this))))})

(def def-unparse
  {:unparse (fn [this]
              `(def ~(proto/unparse (:sym this))
                 ~(proto/unparse (:init this))))})

(def def-walkable
  {:walk-expr (fn [this f ctor]
                (ctor (f (:sym this)) (f (:init this))))})

(extend-group (DefCps DefSrs DefTriv)
  proto/PGatherFreeVars
    def-gather-free-vars

  proto/PUnparse
    def-unparse
 
  proto/PWalkable
    def-walkable)
