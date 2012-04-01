;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  1 Apr 2012
;; 
;; Defines the If record (both triv and srs variants) for the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.if
  (:require [clojure-tco.expr.pexpr :as pexpr])
  (:require [clojure-tco.expr.cont :as cont])
  (:import [clojure_tco.expr.cont
            Cont AppCont])
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(declare IfTriv IfSrs IfCps)

(def if-base
  {:walk-expr   (fn [this f & args]
                  (let [TEST (apply f (:test this) args)
                        CONSEQ (apply f (:conseq this) args)
                        ALT (apply f (:alt this) args)]
                    (IfCps. TEST CONSEQ ALT)))
   :abstract-k  (fn [this apply-k] (walk-expr this apply-k))
   :thunkify    (fn [this] (walk-expr this thunkify))})

(defrecord IfTriv [test conseq alt])

(extend IfTriv
  pexpr/PExpr
  (merge {:triv? (fn [this] true)
          :cps   (fn [this & k] (apply walk-expr this k))}
         if-base))

(defrecord IfSrs [test conseq alt])

(extend IfSrs
  pexpr/PExpr
  (merge {:triv? (fn [this] false)
          :cps   (fn [this & k]
                   (let [[k] k]
                     (let [CONSEQ (apply cps (:conseq this) k)
                           ALT (apply cps (:alt this) k)]
                       (if (triv? (:test this))
                           (IfCps. (:test this) CONSEQ ALT)
                           (let [s (new-var 's)
                                 K-if (IfCps. s CONSEQ ALT)
                                 K-body (AppCont. s K-if)
                                 K (Cont. s K-body)]
                             (cps (:test this) K))))))}
         if-base))

(defrecord IfCps [test conseq alt])

(extend IfCps
  pexpr/PExpr
  (merge {:triv? (fn [this] true)
          :cps   (fn [this & k] this)}
         if-base))
