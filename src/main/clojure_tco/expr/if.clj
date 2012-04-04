;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the If record (both triv and srs variants) for the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.if
  (:require [clojure-tco.protocol
             [pwalkable :as pwalkable]
             [pcps :as pcps]
             [pthunkify :as pthunkify]])
  (:require [clojure-tco.expr.cont :as cont])
  (:import [clojure_tco.expr.cont
            Cont AppCont])
  (:require [clojure-tco.util.new-var :as new-var]))

(declare thunkify triv? cps)

(defrecord IfCps [test conseq alt]
  pthunkify/PThunkify
  (thunkify [this]
    (let [ctor #(IfCps. %1 %2 %3)]
      (pwalkable/walk-expr this thunkify ctor))))

(defrecord IfTriv [test conseq alt]
  pcps/PCps
  (triv? [_] true)
  (cps [this]
    (let [ctor #(IfCps. %1 %2 %3)]
      (pwalkable/walk-expr this pcps/cps ctor)))
  (cps [this _] (pcps/cps this)))

(defrecord IfSrs [test conseq alt]
  pcps/PCps
  (triv? [_] false)
  (cps [_]
    (throw
     (Exception. (str "Attempt to CPS serious 'if' expression as trivial"))))
  (cps [this k]
    (let [test (:test this)
          CONSEQ (pcps/cps (:conseq this) k)
          ALT (pcps/cps (:alt this) k)]
      (if (triv? test)
          (IfCps. test CONSEQ ALT)
          (let [s (new-var/new-var 's)
                K-body (IfCps. s CONSEQ ALT)
                K (Cont. s K-body)]
            (pcps/cps test K))))))

(def if-walkable
  {:walk-expr (fn
                ([this f c] (pwalkable/walk-expr this f c nil))
                ([this f c args]
                   (let [TEST (apply f (:test this) args)
                         CONSEQ (apply f (:conseq this) args)
                         ALT (apply f (:alt this) args)]
                     (c TEST CONSEQ ALT))))})

(extend IfTriv
  pwalkable/PWalkable
  if-walkable)

(extend IfCps
  pwalkable/PWalkable
  if-walkable)
