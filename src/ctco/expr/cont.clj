;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified 11 Sep 2012
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
  (:use [ctco.util :only (extend-group)])
  (:import [ctco.expr.thunk
            Thunk]))

(defrecord Cont [arg body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) nil))
  
  proto/PUnparse
    (unparse [this]
      `(fn [~(proto/unparse (:arg this))] ~(proto/unparse (:body this))))

  proto/PWalkable
    (walk-expr [this f _]
      (Cont. (:arg this) (f (:body this)))))

(defrecord AppContAbs [app-k cont val]
  proto/PUnparse
    (unparse [this]
      `(~(proto/unparse (:app-k this))
        ~(proto/unparse (:cont this))
        ~(proto/unparse (:val this))))

    proto/PWalkable
      (walk-expr [this f _]
        (AppContAbs. (:app-k this) (f (:cont this)) (f (:val this)))))

(defrecord AppCont [cont val]
  proto/PAbstractK
    (abstract-k [this app-k]
      (AppContAbs.
       app-k
       (proto/abstract-k (:cont this) app-k)
       (proto/abstract-k (:val this) app-k)))
  
  proto/PUnparse
    (unparse [this]
      `(~(proto/unparse (:cont this)) ~(proto/unparse (:val this))))

    proto/PWalkable
      (walk-expr [this f _]
        (AppCont. (f (:cont this)) (f (:val this)))))

(def cont-abstract-k
  {:abstract-k (fn [this app-k]
                 (proto/walk-expr this #(proto/abstract-k % app-k) nil))})

(def cont-alpha-rename
  {:alpha-rename (fn [this old new]
                   (proto/walk-expr this #(proto/alpha-rename % old new) nil))})

(def cont-thunkify
  {:thunkify (fn [this]
               (proto/walk-expr this proto/thunkify nil))})

(extend-group (AppContAbs AppCont)
  proto/PAlphaRename
    cont-alpha-rename)

(extend-group (Cont AppContAbs AppCont)
  ;; NB: One might argue that since we're using functions to represent
  ;; NB: continuations, we should alpha-rename them as such. Both because this
  ;; NB: representation is internal and is based on a gensym, we should have to
  ;; NB: worry with it.
  proto/PAlphaRename
    cont-alpha-rename
  
  proto/PThunkify
    cont-thunkify)
