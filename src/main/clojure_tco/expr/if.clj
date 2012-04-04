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
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(declare IfTriv IfSrs IfCps)

(def if-walkable
  {:walk-expr (fn walk-expr
                ([this f c] (walk-expr this f c nil))
                ([this f c args]
                   (let [TEST (apply f (:test this) args)
                         CONSEQ (apply f (:conseq this) args)
                         ALT (apply f (:alt this) args)]
                     (c TEST CONSEQ ALT))))})

(defrecord IfTriv [test conseq alt]
  pwalkable/PWalkable
  walk-expr

  pcps/PCps
  (triv? [_] true)
  (cps [this]
    (let [out-type #(IfCps. %)]
      (walk-expr this cps out-type)))
  (cps [this _] (cps this)))

(defrecord IfSrs [test conseq alt]
  pcps/PCps
  (triv? [_] false)
  (cps [_]
    (throw
     (Exception. (str "Attempt to CPS serious 'if' expression as trivial"))))
  (cps [this k]
    (let [test (:test this)
          CONSEQ (cps (:conseq this) k)
          ALT (cps (:alt this) k)]
      (if (triv? test)
          (IfCps. test CONSEQ ALT)
          (let [s (new-var 's)
                K-body (IfCps. s CONSEQ ALT)
                K (Cont. s K-body)]
            (cps test K))))))

(defrecord IfCps [test conseq alt]
  pwalkable/PWalkable
  walk-expr

  pthunkify/PThunkify
  (thunkify [this]
    (let [out-type #(IfCps. %)]
      (walk-expr thunkify out-type))))
