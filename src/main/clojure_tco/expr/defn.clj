;;----------------------------------------------------------------------
;; File defn.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the record type for 'defn' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.defn
  (:require [clojure-tco.protocol
             [pcps :as pcps]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.fn]
            [clojure-tco.expr.thunk]
            [clojure-tco.util.new-var :as new-var])
  (:import [clojure_tco.expr.fn Fn]
           [clojure_tco.expr.thunk Thunk]))

(defrecord Defn [name func]
  pcps/PCps
  (triv? [_] true)
  (cps [this]
    (let [FUNC (pcps/cps (:func this))]
      (Defn. (:name this) FUNC)))
  (cps [this k] (pcps/cps this))

  pthunkify/PThunkify
  (thunkify [this]
    (let [FUNC (pthunkify/thunkify (:func this))]
      (Defn. (:name this) FUNC))))