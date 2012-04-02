;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  2 Apr 2012
;; 
;; Defines the SimpleOp record types for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.simple-op
  (:require [clojure-tco.expr.pexpr :as pexpr])
  (:require [clojure-tco.expr.cont :as cont])
  (:import [clojure_tco.expr.cont
            Cont AppCont])
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(declare SimpleOpTriv SimpleOpSrs SimpleOpCps)

(def simple-op-base
  {:walk-expr   (fn [this f & args]
                  (let [OPND* (map #(apply f % args) (:opnd* this))]
                    (SimpleOpCps. (:op this) OPND*)))
   :thunkify    (fn [this]
                  (walk-expr this thunkify))})

(defrecord SimpleOpTriv [op opnd*])

(extend SimpleOpTriv
  pexpr/PExpr
  (merge {:triv? (fn [_] true)
          :cps   (fn [this & _] (walk-expr this cps (:opnd* this)))}
         simple-op-base))

(defrecord SimpleOpSrs [op opnd*])

(extend SimpleOpSrs
  pexpr/PExpr
  (merge {:triv? (fn [_] false)
          :cps   (fn [this & k]
                   (let [[k] k]
                     (loop [opnd* (:opnd* this)
                            k k]
                       (let []))))}
         simple-op-base))

(defrecord SimpleOpCps [op opnd*])

(extend SimpleOpCps
  pexpr/PExpr
  (merge {:triv? (fn [_] true)
          :cps   (fn [this & _] this)}
         simple-op-base))
