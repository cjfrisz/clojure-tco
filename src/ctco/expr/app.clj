;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 11 Sep 2012
;; 
;; Defines the App record type for function application in the Clojure
;; TCO compiler.
;;
;; It implements the following protocols:
;;      PAbstractK:
;;              Simply recurs over the rator and rand*
;;
;;      PUnparse:
;;              Unparses (recursively) the sytax for the expression as
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

  proto/PAlphaRename
    (alpha-rename [this old new]
      (proto/walk-expr this #(proto/alpha-rename % old new) nil))
    
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(cps-rand* [rand-in* rand-out*]
                (if (nil? (seq rand-in*))
                    (conj rand-out* k)
                    (let [fst (first rand-in*)
                          nxt (next rand-in*)]
                      (if (util/trivial? fst)
                          (let [FST (proto/cps-triv fst)
                                RAND-OUT* (conj rand-out* FST)]
                            (recur nxt RAND-OUT*))
                          (let [s (util/new-var 's)
                                RAND-OUT* (conj rand-out* s)
                                NXT (cps-rand* nxt RAND-OUT*)
                                K (Cont. s NXT)]
                            (proto/cps-srs fst K))))))]
        (let [RAND* (cps-rand* (:rand* this) [])]
          (if (util/trivial? (:rator this))
              (let [RATOR (proto/cps-triv (:rator this))]
                (App. RATOR RAND*))
              (let [s (util/new-var 's)
                    cont (Cont. s RAND*)]
                (proto/PCpsSrs (:rator this) cont))))))
    
  proto/PThunkify
    (thunkify [this]
      (let [THIS (proto/walk-expr this proto/thunkify nil)]
        (Thunk. THIS)))

  proto/PUnparse
    (unparse [this]
      (let [rator (proto/unparse (:rator this))
            rand* (map proto/unparse (:rand* this))]
        `(~rator ~@rand*)))

  proto/PWalkable
    (walk-expr [this f _]
      (App. (f (:rator this))
            (reduce #(conj %1 (f %2)) [] (:rand* this))))
