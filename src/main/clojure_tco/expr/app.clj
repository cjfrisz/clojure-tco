;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 15 Apr 2012
;; 
;; Defines the record types for function application in the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.app
  (:require [clojure-tco.expr.cont]
            [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pemit :as pemit]
             [pthunkify :as pthunkify]
             [pwalkable :as pwalkable]]
            [clojure-tco.util
             [new-var :as nv]])
  (:import [clojure_tco.expr.cont
            Cont AppCont]))

(defrecord App [rator rand*]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (pwalkable/walk-expr this #(pabs-k/abstract-k % app-k) nil))

  pemit/PEmit
    (emit [this]
      (let [rator (pemit/emit (:rator this))
            rand* (map pemit/emit (:rand* this))]
          `(~rator ~@rand*)))
  
  srs/PCpsSrs
    (cps [this k]
      (letfn [(cps-rator [rator]
                (condp extends? (type rator)
                  triv/PCpsTriv (triv/cps rator)
                  srs/PCpsSrs (srs/cps rator k)))
              (cps-rand* [pre-rand* post-rand* k]
                (if (nil? (seq pre-rand*))
                    (conj post-rand* k)
                    (let [fst (first pre-rand*)
                          nxt (next pre-rand*)]
                      (if (extends? triv/PCpsTriv (type fst))
                          (let [FST (triv/cps fst)
                                POST-RAND* (conj post-rand* FST)]
                            (recur nxt POST-RAND* k))
                          (let [s (nv/new-var 's)
                                POST-RAND* (conj post-rand* s)
                                NXT (cps-rand* nxt POST-RAND* k)
                                K (Cont. s NXT)]
                            (srs/cps fst K))))))]
        (let [RATOR (cps-rator (:rator this))
              RAND* (cps-rand* (:rand* this) [] k)]
          (App. RATOR RAND*))))

  pthunkify/PThunkify
    (thunkify [this] (pwalkable/walk-expr this pthunkify/thunkify nil))

  pwalkable/PWalkable
    (walk-expr [this f _]
      (let [RATOR (f (:rator this))
            RAND* (map f (:rand* this))
            RAND* (into [] RAND*)]
        (App. RATOR RAND*))))
