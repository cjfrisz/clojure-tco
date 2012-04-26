;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the record types for function application in the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.app
  (:require [ctco.expr.cont]
            [ctco.protocol :as proto]
            [ctco.util.new-var :as nv])
  (:import [ctco.expr.cont
            Cont AppCont]))

(defrecord App [rator rand*]
  proto/PAbstractK
    (abstract-k [this app-k]
      (proto/walk-expr this #(proto/abstract-k % app-k) nil))

  proto/PEmit
    (emit [this]
      (let [rator (proto/emit (:rator this))
            rand* (map proto/emit (:rand* this))]
          `(~rator ~@rand*)))
  
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(cps-rator [rator]
                (condp extends? (type rator)
                  proto/PCpsTriv (proto/cps-triv rator)
                  proto/PCpsSrs (proto/cps-srs rator k)))
              (cps-rand* [pre-rand* post-rand* k]
                (if (nil? (seq pre-rand*))
                    (conj post-rand* k)
                    (let [fst (first pre-rand*)
                          nxt (next pre-rand*)]
                      (if (extends? proto/PCpsTriv (type fst))
                          (let [FST (proto/cps-triv fst)
                                POST-RAND* (conj post-rand* FST)]
                            (recur nxt POST-RAND* k))
                          (let [s (nv/new-var 's)
                                POST-RAND* (conj post-rand* s)
                                NXT (cps-rand* nxt POST-RAND* k)
                                K (Cont. s NXT)]
                            (proto/cps-srs fst K))))))]
        (let [RATOR (cps-rator (:rator this))
              RAND* (cps-rand* (:rand* this) [] k)]
          (App. RATOR RAND*))))

  proto/PThunkify
    (thunkify [this] (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
    (walk-expr [this f _]
      (let [RATOR (f (:rator this))
            RAND* (map f (:rand* this))
            RAND* (into [] RAND*)]
        (App. RATOR RAND*))))
