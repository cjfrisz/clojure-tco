;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the record types for function application in the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.app
  (:require [clojure-tco.protocol
             [pwalkable :as pwalkable]
             [pcps :as pcps] 
             [pthunkify :as pthunkify]]
            [clojure-tco.expr.cont]
            [clojure-tco.util
             [new-var :as new-var]])
  (:import [clojure_tco.expr.cont
            Cont AppCont]))

(defrecord App [rator rand*]
  pwalkable/PWalkable
    (walk-expr [this f c] (pwalkable/walk-expr this f c nil))
    (walk-expr [this f c arg*]
      (let [RATOR (apply f (:rator this) arg*)
            RAND* (map #(apply f % arg*) (:rand* this))]
        (c RATOR RAND*)))

  pcps/PCps
    (triv? [_] false)
    (cps [this]
      (throw
       (Exception.
        (str "Attempt to CPS App without continuation argument."))))
    (cps [this k]
      (let [RATOR (pcps/cps (:rator this) k)]
        (letfn [(cps-app [pre-rand* post-rand* k]
                  (if (nil? (seq pre-rand*))
                      (let [RAND* (conj post-rand* k)]
                        (App. RATOR RAND*))
                      (let [fst (first pre-rand*)
                            rst (rest pre-rand*)]
                        (if (pcps/triv? fst)
                            (let [FST (pcps/cps fst)
                                  POST-RAND* (conj post-rand* FST)]
                              (recur rst POST-RAND* k))
                            (let [s (new-var/new-var 's)
                                  POST-RAND* (conj post-rand* s)
                                  RST (cps-app rst POST-RAND* k)
                                  K (Cont. s RST)]
                              (pcps/cps fst K))))))]
          (cps-app (:rand* this) [] k))))

  pthunkify/PThunkify
    (thunkify [this]
      (let [ctor #(App. %1 %2)]
        (pwalkable/walk-expr this pthunkify/thunkify ctor))))
