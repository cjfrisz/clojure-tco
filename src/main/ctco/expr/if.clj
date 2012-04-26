;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the If record (triv, srs, and cps variants) for the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.if
  (:require [ctco.expr.cont :as cont]
            [ctco.protocol :as proto]
            [ctco.util.new-var :as new-var])
  (:import [ctco.expr.cont
            Cont AppCont]))

(defrecord IfCps [test conseq alt]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [ctor #(IfCps. %1 %2 %3)]
        (proto/walk-expr this #(proto/abstract-k % app-k) ctor)))
  
  proto/PThunkify
    (thunkify [this]
      (let [ctor #(IfCps. %1 %2 %3)]
        (proto/walk-expr this proto/thunkify ctor))))

(defrecord IfTriv [test conseq alt]
  proto/PCpsTriv
  (cps-triv [this]
    (let [ctor #(IfCps. %1 %2 %3)]
      (proto/walk-expr this proto/cps-triv ctor)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(IfTriv. %1 %2 %3)]
        (proto/walk-expr this proto/thunkify ctor))))

(defrecord IfSrs [test conseq alt]
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(cps-if [expr]
                (condp extends? (type expr)
                  proto/PCpsTriv (let [EXPR (proto/cps-triv expr)]
                                  (AppCont. k EXPR))
                  proto/PCpsSrs (proto/cps-srs expr k)))]
        (let [test (:test this)
              CONSEQ (cps-if (:conseq this))
              ALT (cps-if (:alt this))]
          (if (extends? proto/PCpsTriv (type test))
              (let [TEST (proto/cps-triv test)]
                (IfCps. TEST CONSEQ ALT))
              (let [s (new-var/new-var 's)
                    K-body (IfCps. s CONSEQ ALT)
                    K (Cont. s K-body)]
                (proto/cps-srs test K))))))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(IfSrs. %1 %2 %3)]
        (proto/walk-expr this proto/thunkify ctor))))

(def if-emit
  {:emit (fn [this]
           (let [test (proto/emit (:test this))
                 conseq (proto/emit (:conseq this))
                 alt (proto/emit (:alt this))]
             `(if ~test ~conseq ~alt)))})

(def if-walkable
  {:walk-expr (fn [this f ctor]
                (let [TEST (f (:test this))
                      CONSEQ (f (:conseq this))
                      ALT (f (:alt this))]
                  (ctor TEST CONSEQ ALT)))})

(extend IfCps
  proto/PEmit
    if-emit

  proto/PWalkable
    if-walkable)

(extend IfSrs
  proto/PEmit
    if-emit)

(extend IfTriv
  proto/PEmit
    if-emit

  proto/PWalkable
    if-walkable)


