;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 20 Oct 2012
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
;;              Maps thunkify over the operands of the expression. Uses
;;              the walk-expr function provided by PWalkable.
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
;;              Maps thunkify over the operands of the expression. Uses
;;              the walk-expr function provided by PWalkable.
;;
;;      PUnRecurify:
;;              Maps unrecurify over the operands of the
;;              expression. Uses the walk-expr function provided by
;;              PWalkable. 
;;
;; IfTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Maps cps-triv over the operands of the expression. Uses
;;              the walk-expr function provided by PWalkable.
;;
;;      PThunkify:
;;              Maps thunkify over the operands of the expression. Uses
;;              the walk-expr function provided by PWalkable.
;;
;;      PUnRecurify:
;;              Maps unrecurify over the operands of the
;;              expression. Uses the walk-expr function provided by
;;              PWalkable. 
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
;;              expression. Uses the walk-expr function provided by
;;              PWalkable. 
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

  proto/PRecurify
  (recurify [this name tail?]
    (proto/walk-expr this #(proto/recurify % nil false) #(SimpleOpCps. %1 %2)))
     
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(SimpleOpCps. %1 %2))))

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

  proto/PRecurify
  (recurify [this name tail?]
    (proto/walk-expr this #(proto/recurify % nil false) #(SimpleOpSrs. %1 %2)))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(SimpleOpSrs. %1 %2)))

  proto/PUnRecurify
  (unrecurify [this name]
    (proto/walk-expr this #(proto/unrecurify % name) #(SimpleOpSrs. %1 %2))))

(defrecord SimpleOpTriv [op opnd*]
  proto/PCpsTriv
  (cps-triv [this]
    (proto/walk-expr this proto/cps-triv #(SimpleOpCps. %1 %2)))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(SimpleOpTriv. %1 %2)))    

  proto/PRecurify
  (recurify [this name tail?]
    (proto/walk-expr this #(proto/recurify % nil false) #(SimpleOpTriv. %1 %2)))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(SimpleOpTriv. %1 %2)))

  proto/PUnRecurify
  (unrecurify [this name]
    (proto/walk-expr this #(proto/unrecurify % name) #(SimpleOpTriv. %1 %2))))                        

(util/extend-multi (SimpleOpCps SimpleOpSrs SimpleOpTriv)
  proto/PUnparse
  (unparse [this] `(~(:op this) ~@(map proto/unparse (:opnd* this))))
  
  proto/PWalkable
  (walk-expr [this f ctor] (ctor (:op this) (mapv f (:opnd* this)))))

