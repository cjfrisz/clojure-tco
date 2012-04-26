;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.fn
  (:require [ctco.expr
             cont thunk]
            [ctco.protocol :as proto]
            [ctco.util.new-var :as nv])
  (:import [ctco.expr.cont
            Cont AppCont]
           [ctco.expr.thunk
            Thunk]))

(defrecord Fn [fml* body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (proto/abstract-k (:body this) app-k)]
        (Fn. (:fml* this) BODY)))

  proto/PEmit
    (emit [this]
      (let [fml* (map proto/emit (:fml* this))
            FML* (into [] fml*)
            body (proto/emit (:body this))]
        `(fn ~FML* ~body)))
  
  proto/PCpsTriv
    (cps-triv [this]
      (let [k (nv/new-var 'k)]
        (let [FML* (conj (:fml* this) k)
              BODY (condp extends? (type (:body this))
                       proto/PCpsTriv (AppCont. k (proto/cps-triv body))
                       proto/PCpsSrs (proto/cps-srs body k))]
          (Fn. FML* BODY))))

  proto/PThunkify
    (thunkify [this]
      (let [BODY (Thunk. (:body this))]
        (Fn. (:fml* this) BODY))))
