;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the Cont, AppCont, and AppContAbs record types for
;; continuations, continuation application, and continuation
;; application abstracted with respect to continuations, respectively.
;;
;; Cont implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(fn [~arg] ~body). Thus it uses higher-order functions
;;              to represent continuations.
;;
;;      PWalkable:
;;              Applies the given function to the body of Cont,
;;              returning a new Cont record.
;;
;; AppCont implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~cont ~val).
;;
;;      PWalkable:
;;              Applies the given function to the continuation and value
;;              of the continuation application, returning a new AppCont
;;              record with the resulting continuation and value.
;;
;; Cont and AppCont have the same implementations for the following
;; protocols:
;;
;;      PLoadTrampoline:
;;              Applies the load-tramp function to each subexpression.
;;              Uses the walk-expr function provided by PWalkable.
;;
;;      PThunkify:
;;              Applies the thunkify function to each subexpression.
;;              Uses the walk-expr function provided by PWalkable.
;;----------------------------------------------------------------------

(ns ctco.expr.cont
  (:require [ctco.protocol :as proto]
            [ctco.util :as util]))

(defrecord Cont [arg body]  
  proto/PUnparse
    (unparse [this]
      `(with-meta
         (fn [~(proto/unparse (:arg this))] ~(proto/unparse (:body this)))
         {:kont true}))

  proto/PWalkable
    (walk-expr [this f _]
      (Cont. (:arg this) (f (:body this)))))

(defrecord AppCont [cont val]
  proto/PUnparse
    (unparse [this]
      (let [cont (proto/unparse (:cont this))
            val (proto/unparse (:val this))]
        `(~cont ~val)))

  proto/PWalkable
    (walk-expr [this f _]
      (AppCont. (f (:cont this)) (f (:val this)))))

(def cont-load-tramp
  {:load-tramp (fn [this tramp]
                 (proto/walk-expr this #(proto/load-tramp % tramp)
                                  nil))})

(def cont-thunkify
  {:thunkify (fn [this]
               (proto/walk-expr this proto/thunkify nil))})

(util/extend-group (Cont AppCont)
  proto/PLoadTrampoline
  cont-load-tramp

  proto/PThunkify
  cont-thunkify)
