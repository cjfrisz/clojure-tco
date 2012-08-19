;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Fn record type for representing 'fn' expressions in the
;; Clojure TCO compiler.
;;
;; It implements the following protocols:
;;
;;      PAbstractK:
;;              Recursively applies abstract-k to the body expression,
;;              returning a new Fn record.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(fn ~fml* body).
;;
;;      PCpsTriv:
;;              Applies the CPS transformation to the body expression
;;              and extends the formal parameters list with an
;;              additional 'k' argument for the continuation.
;;
;;      PThunkify:
;;              Simply calls thunkify on the body and returns a new Fn
;;              record with that body value. 
;;----------------------------------------------------------------------

(ns ctco.expr.fn
  (:require [ctco.expr
             cont thunk]
            [ctco.protocol :as proto]
            [ctco.util :as util])
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
      (let [fml* (vec (map proto/emit (:fml* this)))
            body (proto/emit (:body this))]
        `(fn ~fml* ~body)))
  
  proto/PCpsTriv
    (cps-triv [this]
      (let [k (util/new-var 'k)]
        (let [FML* (conj (:fml* this) k)
              BODY (condp extends? (type (:body this))
                       proto/PCpsTriv (AppCont. k (proto/cps-triv body))
                       proto/PCpsSrs (proto/cps-srs body k))]
          (Fn. FML* BODY))))

  proto/PThunkify
    (thunkify [this]
      (let [BODY (proto/thunkify (:body this))]
        (Fn. (:fml* this) BODY))))
