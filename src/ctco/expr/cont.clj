;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified 27 Sep 2012
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
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
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
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~cont ~val).
;;
;;      PThunkify:
;;              Simply returns the expression.
;;
;; AppContAbs implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
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
  
  proto/PUnparse
    (unparse [this]
      (let [arg (proto/unparse (:arg this))
            body (proto/unparse (:body this))]
        `(with-meta (fn [~arg] ~body) {:kont true})))

  proto/PThunkify
    (thunkify [this]
      (let [BODY (proto/thunkify (:body this))]
        (Cont. (:arg this) BODY))))

(defrecord AppContAbs [app-k cont val]
  proto/PUnparse
    (unparse [this]
      (let [app-k (proto/unparse (:app-k this))
            cont (proto/unparse (:cont this))
            val (proto/unparse (:val this))]
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

  proto/PUnparse
    (unparse [this]
      (let [cont (proto/unparse (:cont this))
            val (proto/unparse (:val this))]
        `(~cont ~val)))

  proto/PThunkify
    (thunkify [this]
      (let [CONT (proto/thunkify (:cont this))
            VAL (proto/thunkify (:val this))]
        (AppCont. CONT VAL))))
