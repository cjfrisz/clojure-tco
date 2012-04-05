;;----------------------------------------------------------------------
;; File simple_op.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the SimpleOp record types for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.simple-op
  (:require [clojure-tco.protocol
             [pwalkable :as pwalkable]
             [pcps :as pcps]
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.cont :as cont]
            [clojure-tco.util.new-var :as new-var])
  (:import [clojure_tco.expr.cont
            Cont AppCont]))

(defrecord SimpleOpCps [op opnd*]
  pthunkify/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpCps. %1 %2)]
        (pwalkable/walk-expr this pthunkify/thunkify ctor))))

(defrecord SimpleOpTriv [op opnd*]
  pcps/PCps
    (triv? [_] true)
    (cps [this]
      (let [ctor #(SimpleOpCps. %1 %2)]
        (pwalkable/walk-expr this pcps/cps ctor)))
    (cps [this _] (pcps/cps this))

  pthunkify/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpTriv. %1 %2)]
        (pwalkable/walk-expr this pthunkify/thunkify ctor))))

(defrecord SimpleOpSrs [op opnd*]
  pcps/PCps
    (triv? [_] false)
    (cps [this]
      (throw
       (Exception.
        (str "Attempt to CPS SimpleOpSrs without continuation argument."))))
    (cps [this k]
      (letfn [(cps-op [pre-opnd* post-opnd* k]
                (if (nil? (seq pre-opnd*))
                    (let [op (SimpleOpCps. (:op this) post-opnd*)]
                      (AppCont. k op))
                    (let [fst (first pre-opnd*)
                          rst (rest pre-opnd*)]
                      (if (pcps/triv? fst)
                          (let [FST (pcps/cps fst)
                                POST-OPND* (conj post-opnd* FST)]
                            (recur rst k POST-OPND*))
                          (let [s (new-var/new-var 's)
                                POST-OPND* (conj post-opnd* s)
                                RST (cps-op rst POST-OPND* k)
                                K (Cont. s RST)]
                            (pcps/cps fst K))))))]
        (cps-op (:opnd* this) [] k)))

  pthunkify/PThunkify
    (thunkify [this]
      (let [ctor #(SimpleOpSrs. %1 %2)]
        (pwalkable/walk-expr this pthunkify/thunkify ctor))))

(def simple-op-walk
  {:walk-expr (fn [this f ctor]
                (let [OPND* (map #(f %) (:opnd* this))]
                  (ctor (:op this) OPND*)))})

(extend SimpleOpCps
  pwalkable/PWalkable
  simple-op-walk)

(extend SimpleOpTriv
  pwalkable/PWalkable
  simple-op-walk)

(extend SimpleOpSrs
  pwalkable/PWalkable
  simple-op-walk)
