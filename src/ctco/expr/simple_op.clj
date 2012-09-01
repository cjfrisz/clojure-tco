;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 31 Aug 2012
;; 
;; Defines the SimpleOpSrs, SimpleOpTriv, and SimpleOpCps record types
;; for representing operations using simple primitives, i.e.
;; arithmetic and relational operators or simple type predicates.
;; SimpleOpSrs and SimpleOpTriv correspond to primitive operations
;; that are "serious" or "trivial" with respect to the Danvy-style CPS
;; algorithm, respectively. The SimpleOpCps record type corresponds to
;; primitive operations which have undergone the CPS transformation.
;;
;; IfCps implements the following protocols:
;;
;;      PAbstractK:
;;              Maps abstract-k over the operands of the expression.
;;
;;      PThunkify:
;;              Maps thunkify over the operands of the expression.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~op ~@opnd*), where opnd* is the vector of operands.
;;
;;      PWalkable:
;;              Maps the given function over the operands of the
;;              expression. 
;;
;; IfSrs implements the following protocols:
;;
;;      PCpsSrs:
;;              Applies the Danvy-style CPS transformation to the
;;              primitive operation. Essentially, for each trivial
;;              subexpression, it is CPSed and left in place. For each
;;              serious subexpression, it is pulled out of the
;;              expression, evaluated, and the original application
;;              expression is placed inside a continuation with the
;;              subexpression replaced with a variable.
;;
;;      PThunkify:
;;              Maps thunkify over the operands of the expression.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~op ~@opnd*)
;;
;;      PWalkable:
;;              Maps the given function over the operands of the
;;              expression. 
;;
;; IfTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Maps cps-triv over the operands of the expression.
;;
;;      PThunkify:
;;              Maps thunkify over the operands of the expression.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~op ~@opnd*)
;;
;;      PWalkable:
;;              Maps the given function over the operands of the
;;              expression. 
;;----------------------------------------------------------------------

(ns ctco.expr.simple-op
  (:require [ctco.protocol :as proto]
            [ctco.expr.cont :as cont]
            [ctco.util :as util])
  (:import [ctco.expr.cont
            Cont AppCont]))

(defrecord SimpleOpCps [op opnd*]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [ctor #(SimpleOpCps. %1 %2)]
        (proto/walk-expr this #(proto/abstract-k % app-k) ctor)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpCps. %1 %2)]
        (proto/walk-expr this proto/thunkify ctor))))

(defrecord SimpleOpTriv [op opnd*]
  proto/PCpsTriv
    (cps-triv [this]
      (let [ctor #(SimpleOpCps. %1 %2)]
        (proto/walk-expr this proto/cps-triv ctor)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpTriv. %1 %2)]
        (proto/walk-expr this proto/thunkify ctor))))

(defrecord SimpleOpSrs [op opnd*]
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(cps-op [pre-opnd* post-opnd* k]
                (if (nil? (seq pre-opnd*))
                    (let [OP (SimpleOpCps. (:op this) post-opnd*)]
                      (AppCont. k OP))
                    (let [fst (first pre-opnd*)
                          rst (rest pre-opnd*)]
                      (if (extends? proto/PCpsTriv (type fst))
                          (let [FST (proto/cps-triv fst)
                                POST-OPND* (conj post-opnd* FST)]
                            (recur rst POST-OPND* k))
                          (let [s (util/new-var 's)
                                POST-OPND* (conj post-opnd* s)
                                RST (cps-op rst POST-OPND* k)
                                K (Cont. s RST)]
                            (proto/cps-srs fst K))))))]
        (cps-op (:opnd* this) [] k)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpSrs. %1 %2)]
        (proto/walk-expr this proto/thunkify ctor))))

(def simple-op-unparse
  {:unparse (fn [this]
              (let [op (:op this)
                    opnd* (map proto/unparse (:opnd* this))]
                `(~op ~@opnd*)))})

(def simple-op-walk
  {:walk-expr (fn [this f ctor]
                (let [OPND* (vec (map #(f %) (:opnd* this)))]
                  (ctor (:op this) OPND*)))})

(util/extend-group (SimpleOpCps SimpleOpSrs SimpleOpTriv)
  proto/PUnparse
    simple-op-unparse
  
  proto/PWalkable
    simple-op-walk)
