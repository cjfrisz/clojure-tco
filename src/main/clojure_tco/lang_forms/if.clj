;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 30 Mar 2012
;; 
;; Defines the If record (both triv and srs variants) for the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms.if
  (:require [clojure-tco.lang-forma.expr :as expr])
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(def if-base
  {:expr-walk   (fn [this f & args]
                  )
   :abstract-k  (fn [this apply-k]
                  (let [TEST (abstract-k (:test this) apply-k)
                        CONSEQ (abstract-k (:conseq this) apply-k)
                        ALT (abstract-k (:alt this) apply-k)]
                    ((type this) TEST CONSEQ ALT)))
   :thunkify    (fn [this]
                  (let [TEST (thunkify (:test this))
                        CONSEQ (thunkify (:conseq this))
                        ALT (thunkify (:alt this))]
                    ((type this) TEST CONSEQ ALT)))})

(defrecord IfTriv [test conseq alt])

(extend IfTriv
  expr/Expr
  (merge {:cps (fn [this]
                 (let [TEST (cps (:test this))
                       CONSEQ (cps (:conseq this))
                       ALT (cps (:alt this))]
                   (IfTriv. TEST CONSEQ ALT)))}
         if-base))

(defrecord IfSrs [test conseq alt])

(extend IfSrs
  expr/Expr
  (merge {:cps (fn [this & ])}))