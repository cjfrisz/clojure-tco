;;----------------------------------------------------------------------
;; File defn.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the record type for 'defn' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.defn
  (:require [clojure-tco.protocol
             [pcps-triv :as triv]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.fn]
            [clojure-tco.expr.thunk]
            [clojure-tco.util.new-var :as new-var])
  (:import [clojure_tco.expr.fn Fn]
           [clojure_tco.expr.thunk Thunk]))

(defrecord Defn [name func]
  triv/PCpsTriv
    (cps [this]
      (let [FUNC (triv/cps (:func this))]
        (Defn. (:name this) FUNC)))

  pthunkify/PThunkify
    (thunkify [this]
      (let [FUNC (pthunkify/thunkify (:func this))]
        (Defn. (:name this) FUNC))))