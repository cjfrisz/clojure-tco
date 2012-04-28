;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 28 Apr 2012
;; 
;; Defines the App record type for function application in the Clojure
;; TCO compiler.
;;
;; It implements the following protocols:
;;      PAbstractK:
;;              Simply recurs over the rator and rand*
;;
;;      PEmit:
;;              Emits (recursively) the sytax for the expression as
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
            [ctco.util :as util])
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
      (letfn [(cps-rand* [rand*-in rand*-out rator k]
                (if (nil? (seq rand*-in))
                    (let [RAND*-OUT (conj rand*-out k)]
                      (App. rator RAND*-OUT))
                    (let [fst (first rand*-in)
                          nxt (next rand*-in)]
                      (if (util/trivial? fst)
                          (let [FST (proto/cps-triv fst)
                                RAND*-OUT (conj rand*-out FST)]
                            (recur nxt RAND*-OUT rator k))
                          (let [s (util/new-var 's)
                                RAND*-OUT (conj rand*-out s)
                                NXT (cps-rand* nxt RAND*-OUT rator k)
                                K (Cont. s NXT)]
                            (proto/cps-srs fst K))))))]
        (let [rator (:rator this)
              rand* (:rand* this)]
          (if (util/trivial? rator)
              (let [RATOR (proto/PCpsTriv rator)]
                (cps-rand* rand* [] rator k))
              (let [s (util/new-var 's)
                    app-k (cps-rand* rand* [] s k)
                    cont (Cont. s app-k)]
                (proto/PCpsSrs rator cont))))))

  proto/PThunkify
    (thunkify [this]
      (let [THIS (proto/walk-expr this proto/thunkify nil)]
        (Thunk. THIS)))

  proto/PWalkable
    (walk-expr [this f _]
      (let [RATOR (f (:rator this))
            RAND* (vec (map f (:rand* this)))]
        (App. RATOR RAND*))))
