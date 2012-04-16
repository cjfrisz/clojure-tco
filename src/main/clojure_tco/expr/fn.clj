;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.fn
  (:require [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pemit :as pemit]
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr
             cont thunk]
            [clojure-tco.util.new-var :as nv])
  (:import [clojure_tco.expr.cont
            Cont AppCont]
           [clojure_tco.expr.thunk
            Thunk]))

(defrecord Fn [fml* body]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (pabs-k/abstract-k (:body this) app-k)]
        (Fn. (:fml* this) BODY)))

  pemit/PEmit
    (emit [this]
      (let [fml* (map pemit/emit (:fml* this))
            FML* (into [] fml*)
            body (pemit/emit (:body this))]
        `(fn ~FML* ~body)))
  
  triv/PCpsTriv
    (cps [this]
      (let [k (nv/new-var 'k)]
        (let [FML* (conj (:fml* this) k)
              BODY (condp extends? (type (:body this))
                       triv/PCpsTriv (AppCont. k (triv/cps body))
                       srs/PCpsSrs (srs/cps body k))]
          (Fn. FML* BODY))))

  pthunkify/PThunkify
    (thunkify [this]
      (let [BODY (Thunk. (:body this))]
        (Fn. (:fml* this) BODY))))
