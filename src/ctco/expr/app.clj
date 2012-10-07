;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the App record type for function application in the Clojure
;; TCO compiler.
;;
;; It implements the following protocols:
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
;;      PLoadTrampoline:
;;              Recursively applies load-tramp to the operator and
;;              operands of the expression.
;;
;;      PThunkify:
;;              Recursively thunkifies the rator and each rand* and
;;              puts the result inside of a thunk.
;;
;;      PUnparse:
;;              Unparses (recursively) the sytax for the expression as
;;              `(~rator ~@rand*)
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
  proto/PCpsSrs
  (cps-srs [this k]
    (letfn [(cps-rand* [rand-in* rand-out*]
              (if (nil? (seq rand-in*))
                  (conj rand-out* k)
                  (let [fst (first rand-in*)
                        nxt (next rand-in*)]
                    (if (util/trivial? fst)
                        (recur nxt (conj rand-out* (proto/cps-triv fst)))
                        (let [s (util/new-var "s")]
                          (proto/cps-srs
                           fst
                           (Cont. s (cps-rand* nxt (conj rand-out* s)))))))))]
      (let [RAND* (cps-rand* (:rand* this) [])]
        (if (util/trivial? (:rator this))
            (App. (proto/cps-triv (:rator this)) RAND*)
            (proto/PCpsSrs (:rator this) (Cont. (util/new-var "s") RAND*))))))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))
    
  proto/PThunkify
  (thunkify [this]
    (Thunk. (proto/walk-expr this proto/thunkify nil)))

  proto/PUnparse
  (unparse [this]
    `(~(proto/unparse (:rator this)) ~@(map proto/unparse (:rand* this))))

  proto/PWalkable
  (walk-expr [this f _]
    (App. (f (:rator this)) (mapv f (:rand* this)))))
