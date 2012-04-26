;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the App record type for function application in the Clojure
;; TCO compiler.
;;
;; It implements the following protocols:
;;      PAbstractK:
;;              Simply recurs over the rator and rand*
;;
;;      PEmit:
;;              Emits (recursively) the expression as
;;              `(~rator ~@rand*)
;;
;;      PCpsSrs:
;;              Applies the Danvy-style CPS transformation to the
;;              application expression. Essentially, for each trivial
;;              subexpression, it is CPSed and left in place. For each
;;              serious subexpression, it is pulled out of the
;;              expression, evaluated, and the original application
;;              expression is placed inside a continuation with the
;;              subexpression replaced with a variable.
;;
;;      PThunkify:
;;              Recursively thunkifies the rator and each rand* and
;;              puts the result inside of a thunk.
;;
;;      PWalkable:
;;              Applies the given function to the rator and each rand*
;;              and returns a new App record.
;;----------------------------------------------------------------------

(ns ctco.expr.app
  (:require [ctco.expr
             cont thunk]
            [ctco.protocol :as proto]
            [ctco.util.new-var :as nv])
  (:import [ctco.expr.cont
            Cont AppCont]
           [ctco.expr.thunk
            Thunk]))

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
    (thunkify [this]
      (let [THIS (proto/walk-expr this proto/thunkify nil)]
        (Thunk. THIS)))

  proto/PWalkable
    (walk-expr [this f _]
      (let [RATOR (f (:rator this))
            RAND* (vec (map f (:rand* this)))]
        (App. RATOR RAND*))))
