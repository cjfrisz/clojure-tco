;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified  5 Aug 2012
;; 
;; Defines the Cont, AppCont, and AppContAbs record types for
;; continuations, continuation application, and continuation
;; application abstracted with respect to continuations, respectively.
;;
;; Cont implements the following protocols:
;;
;;      PAbstractK:
;;              Applies abstract-k to the body.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(fn [~arg] ~body). Thus it uses higher-order functions
;;              to represent continuations.
;;
;;      PThunkify:
;;              Simply returns the expression.
;;
;; AppCont implements the following protocols:
;;
;;      PAbstractK:
;;              Recursively applies abstract-k to the continuation and
;;              val, and returns an AppContAbs record with the given
;;              app-k as the function for applying continuations.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(~cont ~val).
;;
;;      PThunkify:
;;              Simply returns the expression.
;;
;; AppContAbs implements the following protocols:
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(~app-k ~cont ~val).
;;
;;      PThunkify:
;;              Simply returns the expression.
;;----------------------------------------------------------------------

(ns ctco.expr.cont
  (:require [ctco.expr.thunk]
            [ctco.protocol :as proto])
  (:import [ctco.expr.thunk
            Thunk]))

(defrecord Cont [arg body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (proto/abstract-k (:body this) app-k)]
        (Cont. (:arg this) BODY)))
  
  proto/PEmit
    (emit [this]
      (let [arg (proto/emit (:arg this))
            body (proto/emit (:body this))]
        `(fn [~arg] ~body)))

  proto/PThunkify
    (thunkify [this]
      (let [BODY (proto/thunkify (:body this))]
        (Cont. (:arg this) BODY))))

(defrecord AppContAbs [app-k cont val]
  proto/PEmit
    (emit [this]
      (let [app-k (proto/emit (:app-k this))
            cont (proto/emit (:cont this))
            val (proto/emit (:val this))]
        `(~app-k ~cont ~val)))

  proto/PThunkify
    (thunkify [this]
      (let [CONT (proto/thunkify (:cont this))
            VAL (proto/thunkify (:val this))]
        (AppContAbs. (:app-k this) CONT VAL))))

(defrecord AppCont [cont val]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [CONT (proto/abstract-k (:cont this) app-k)
            VAL (proto/abstract-k (:val this) app-k)]
        (AppContAbs. app-k CONT VAL)))

  proto/PEmit
    (emit [this]
      (let [cont (proto/emit (:cont this))
            val (proto/emit (:val this))]
        `(~cont ~val)))

  proto/PThunkify
    (thunkify [this]
      (let [CONT (proto/thunkify (:cont this))
            VAL (proto/thunkify (:val this))]
        (AppCont. CONT VAL))))
