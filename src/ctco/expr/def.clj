;;----------------------------------------------------------------------
;; File def.clj
;; Written by Chris
;; 
;; Created 30 Aug 2012
;; Last modified  3 Oct 2012
;; 
;; Defines the DefSrs, DefTriv, and DefCps record types for representing
;; 'def' expression in the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.def
  (:require [ctco.protocol :as proto]
            [ctco.util :as util]))

(defrecord DefCps [sym init]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) #(DefCps. %1 %2)))

  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(DefCps. %1 %2)))
  
  proto/PThunkify
    (thunkify [this]
      (proto/walk-expr this proto/thunkify #(DefCps. %1 %2))))

(defrecord DefSrs [sym init]
  proto/PCpsSrs
    (cps-srs [this k]
      (proto/walk-expr this #(proto/cps-srs % k) #(DefCps. %1 %2)))

  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(DefSrs. %1 %2))))

(defrecord DefTriv [sym init]
  proto/PCpsTriv
    (cps-triv [this]
      (proto/walk-expr this proto/cps-triv #(DefCps. %1 %2)))

  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(DefTriv. %1 %2))))

(def def-unparse
  {:unparse (fn [this]
              `(def ~(proto/unparse (:sym this))
                 ~(proto/unparse (:init this))))})

(def def-walkable
  {:walk-expr (fn [this f ctor]
                (ctor (:sym this) (f (:init this))))})

(util/extend-group (DefCps DefSrs DefTriv)
  proto/PUnparse
    def-unparse
 
  proto/PWalkable
    def-walkable)
