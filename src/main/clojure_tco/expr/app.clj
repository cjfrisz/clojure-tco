;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 11 Apr 2012
;; 
;; Defines the record types for function application in the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.app
  (:require [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pemit :as pemit]
             [pthunkify :as pthunkify]
             [pwalkable :as pwalkable]]
            [clojure-tco.expr.cont]
            [clojure-tco.util
             [new-var :as new-var]])
  (:import [clojure_tco.expr.cont
            Cont AppCont]))

(defrecord App [rator rand*]
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
                          rst (rest pre-rand*)]
                      (if (extends? triv/PCpsTriv (type fst))
                          (let [FST (triv/cps fst)
                                POST-RAND* (conj post-rand* FST)]
                            (recur rst POST-RAND* k))
                          (let [s (new-var/new-var 's)
                                POST-RAND* (conj post-rand* s)
                                RST (cps-rand* rst POST-RAND* k)
                                K (Cont. s RST)]
                            (srs/cps fst K))))))]
        (let [RATOR (cps-rator (:rator this))
              RAND* (cps-rand* (:rand* this) [] k)]
          (App. RATOR RAND*))))

  pemit/PEmit
    (emit [this]
      (let [rator (emit (:rator this))
              rand* (map emit (:rand* this))]
          `(~rator ~@rand*)))

  pthunkify/PThunkify
    (thunkify [this]
      (let [ctor #(App. %1 %2)]
        (pwalkable/walk-expr this pthunkify/thunkify ctor)))

  pwalkable/PWalkable
    (walk-expr [this f c]
      (let [RATOR (f (:rator this))
            RAND* (map #(f %) (:rand* this))]
        (c RATOR RAND*))))
