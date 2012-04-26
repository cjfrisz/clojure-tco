;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the SimpleOp record types for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.simple-op
  (:require [ctco.protocol :as proto]
            [ctco.expr.cont :as cont]
            [ctco.util.new-var :as new-var])
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
                    (let [op (SimpleOpCps. (:op this) post-opnd*)]
                      (AppCont. k op))
                    (let [fst (first pre-opnd*)
                          rst (rest pre-opnd*)]
                      (if (extends? proto/PCpsTriv (type fst))
                          (let [FST (proto/cps-triv fst)
                                POST-OPND* (conj post-opnd* FST)]
                            (recur rst POST-OPND* k))
                          (let [s (new-var/new-var 's)
                                POST-OPND* (conj post-opnd* s)
                                RST (cps-op rst POST-OPND* k)
                                K (Cont. s RST)]
                            (proto/cps-srs fst K))))))]
        (cps-op (:opnd* this) [] k)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpSrs. %1 %2)]
        (proto/walk-expr this proto/thunkify ctor))))

(def simple-op-emit
  {:emit (fn [this]
           (let [op (:op this)
                 opnd* (map proto/emit (:opnd* this))]
             `(~op ~@opnd*)))})

(def simple-op-walk
  {:walk-expr (fn [this f ctor]
                (let [OPND* (vec (map #(f %) (:opnd* this)))]
                  (ctor (:op this) OPND*)))})

(extend SimpleOpCps
  proto/PEmit
    simple-op-emit
  
  proto/PWalkable
    simple-op-walk)

(extend SimpleOpSrs
  proto/PEmit
    simple-op-emit
  
  proto/PWalkable
    simple-op-walk)

(extend SimpleOpTriv
  proto/PEmit
    simple-op-emit
  
  proto/PWalkable
    simple-op-walk)
