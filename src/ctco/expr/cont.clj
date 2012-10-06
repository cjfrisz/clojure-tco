;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified  5 Oct 2012
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
;;----------------------------------------------------------------------

(ns ctco.expr.cont
  (:require [ctco.protocol :as proto]
            [ctco.util :as util]))

(defrecord Cont [arg body]  
  proto/PUnparse
    (unparse [this]
      (let [arg (proto/unparse (:arg this))
            body (proto/unparse (:body this))]
        `(with-meta (fn [~arg] ~body) {:kont true})))

  proto/PThunkify
    (thunkify [this]
      (let [BODY (proto/thunkify (:body this))]
        (Cont. (:arg this) BODY)))

  proto/PWalkable
    (walk-expr [this f _]
      (Cont. (:arg this) (f (:body this)))))

(defrecord AppCont [cont val]
  proto/PUnparse
    (unparse [this]
      (let [cont (proto/unparse (:cont this))
            val (proto/unparse (:val this))]
        `(~cont ~val)))

  proto/PThunkify
    (thunkify [this]
      (let [CONT (proto/thunkify (:cont this))
            VAL (proto/thunkify (:val this))]
        (AppCont. CONT VAL)))

  proto/PWalkable
    (walk-expr [this f _]
      (AppCont. (f (:cont this)) (f (:val this)))))

(def cont-load-tramp
  {:load-tramp (fn [this tramp]
                 (proto/walk-expr this #(proto/load-tramp % tramp) nil))})

(util/extend-group (Cont AppCont)
  proto/PLoadTrampoline
  cont-load-tramp)
