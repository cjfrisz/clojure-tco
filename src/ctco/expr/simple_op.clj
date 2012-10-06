;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  6 Oct 2012
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
;;      PThunkify:
;;              Maps thunkify over the operands of the expression.
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
;; IfTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Maps cps-triv over the operands of the expression.
;;
;;      PThunkify:
;;              Maps thunkify over the operands of the expression.
;;
;; IfCps, IfSrs, and IfTriv use the same implementation for the
;; following protocols:
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
  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(SimpleOpCps. %1 %2)))

  proto/PThunkify
    (thunkify [this]
      (proto/walk-expr this proto/thunkify #(SimpleOpCps. %1 %2))))

(defrecord SimpleOpTriv [op opnd*]
  proto/PCpsTriv
    (cps-triv [this]
      (proto/walk-expr this proto/cps-triv #(SimpleOpCps. %1 %2)))

  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(SimpleOpTriv. %1 %2)))

  proto/PThunkify
    (thunkify [this]
      (proto/walk-expr this proto/thunkify #(SimpleOpTriv. %1 %2))))

(defrecord SimpleOpSrs [op opnd*]
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(cps [pre-opnd* post-opnd* k]
                (if (nil? (seq pre-opnd*))
                    (let [OP (SimpleOpCps. (:op this) post-opnd*)]
                      (AppCont. k OP))
                    (let [fst (first pre-opnd*)
                          rst (rest pre-opnd*)]
                      (if (extends? proto/PCpsTriv (type fst))
                          (recur rst (conj post-opnd* (proto/cps-triv fst)) k)
                          (let [s (util/new-var "s")]
                            (proto/cps-srs
                             fst
                             (Cont. s (cps rst (conj post-opnd* s) k))))))))]
        (cps (:opnd* this) [] k)))

  proto/PLoadTrampoline
    (load-tramp [this tramp]
      (proto/walk-expr this #(proto/load-tramp % tramp) #(SimpleOpSrs. %1 %2)))

  proto/PThunkify
    (thunkify [this]
      (proto/walk-expr this proto/thunkify #(SimpleOpSrs. %1 %2))))

(def simple-op-unparse
  {:unparse (fn [this]
              `(~(:op this) ~@(map proto/unparse (:opnd* this))))})

(def simple-op-walk
  {:walk-expr (fn [this f ctor]
                (ctor (:op this)
                      (reduce (fn [opnd* opnd] (conj opnd* (f opnd)))
                              []
                              (:opnd* this))))})
                        

(util/extend-group (SimpleOpCps SimpleOpSrs SimpleOpTriv)
  proto/PUnparse
    simple-op-unparse
  
  proto/PWalkable
    simple-op-walk)
