;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified 20 Oct 2012
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
;;      PRecurify:
;;              Applies the recurify function to each
;;              subexpression. Since neither a continuation nor a
;;              continuation application is a tail call, 'false' is
;;              passed for the 'tail?' value and 'nil' for the name in
;;              the recursive calls.  Uses the walk-exp function
;;              provided by PWalkable.
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
    `(~(proto/unparse (:cont this)) ~(proto/unparse (:val this))))

  proto/PWalkable
  (walk-expr [this f _]
    (AppCont. (f (:cont this)) (f (:val this)))))

(util/extend-multi (Cont AppCont)
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))

  proto/PRecurify
  (recurify [this name tail?]
    (proto/walk-expr this #(proto/recurify % nil false) nil))
  
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify nil)))
