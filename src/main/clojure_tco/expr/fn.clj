;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.fn
  (:require [clojure-tco.protocol
             [pcps :as pcps]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.thunk]
            [clojure-tco.util.new-var :as new-var])
  (:import [clojure_tco.expr.thunk Thunk]))

(defrecord Fn [fml* body]
  pcps/PCps
  (triv? [_] true)
  (cps [this]
    (let [k (new-var/new-var 'k)]
      (let [FML* (conj (:fml* this) k)
            BODY (pcps/cps (:body this) k)]
        (Fn. FML* BODY))))
  (cps [this _] (pcps/cps this))

  pthunkify/PThunkify
  (thunkify [this]
    (let [BODY (Thunk. (:body this))]
      (Fn. (:fml* this) BODY))))
