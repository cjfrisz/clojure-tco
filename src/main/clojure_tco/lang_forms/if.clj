;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 31 Mar 2012
;; 
;; Defines the If record (both triv and srs variants) for the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms.if
  (:require [clojure-tco.lang-forma.expr :as expr])
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
  expr/PExpr
  (merge {:cps (fn [this & k] (apply walk-expr this k))}
         if-base))

(defrecord IfSrs [test conseq alt])

(extend IfSrs
  expr/PExpr
  (merge {:cps (fn [this & k])}
         if-base))

(defrecord IfCps [test conseq alt])

(extend IfCps
  expr/PExpr
  (merge {:cps (fn [this & k] this)}
         if-base))
