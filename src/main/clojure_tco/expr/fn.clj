;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.fn
  (:require [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.cont]
            [clojure-tco.expr.thunk]
            [clojure-tco.util.new-var :as new-var])
  (:import [clojure_tco.expr.cont
            Cont AppCont]
           [clojure_tco.expr.thunk
            Thunk]))

(defrecord Fn [fml* body]
  triv/PCpsTriv
    (cps [this]
      (let [k (new-var/new-var 'k)]
        (let [FML* (conj (:fml* this) k)
              BODY (condp extends? (type (:body this))
                       triv/PCpsTriv (AppCont. k (triv/cps body))
                       srs/PCpsSrs (srs/cps body k))]
          (Fn. FML* BODY))))

  pthunkify/PThunkify
    (thunkify [this]
      (let [BODY (Thunk. (:body this))]
        (Fn. (:fml* this) BODY))))
