;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified 20 Oct 2012
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
;;      PRecurify:
;;              If the name argument matches the rator and the value of
;;              tail? is true, replaces the application with the recur
;;              form. If not, returns a new application with recurify
;;              applied to the rator with nil and false for the values
;;              of name and tail?, respectively, since the rator cannot
;;              be a tail call. Regardless of whether the application is
;;              a self-recursive tail call, recurify is applied to the
;;              arguments with values nil and false for name and tail?,
;;              respectively.
;;
;;      PThunkify:
;;              Recursively thunkifies the rator and each rand* and
;;              puts the result inside of a thunk.
;;
;;      PUnparse:
;;              Unparses (recursively) the sytax for the expression as
;;              `(~rator ~@rand*)
;;
;;      PUnRecurify
;;              Maps the unrecurify transformation over the rator and
;;              rand* of the application. Uses the walk-expr function
;;              provided by PWalkable.
;;
;;      PWalkable:
;;              Applies the given function to the rator and each rand*
;;              and returns a new App record.
;;----------------------------------------------------------------------

(ns ctco.expr.app
  (:require [ctco.expr
             cont simple thunk]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.cont
            Cont AppCont]
           [ctco.expr.simple
            Simple]
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
                        (let [s (util/new-var 's)]
                          (proto/cps-srs
                           fst
                           (Cont. s (cps-rand* nxt (conj rand-out* s)))))))))]
      (let [RAND* (cps-rand* (:rand* this) [])]
        (if (util/trivial? (:rator this))
            (App. (proto/cps-triv (:rator this)) RAND*)
            (proto/PCpsSrs (:rator this) (Cont. (util/new-var 's) RAND*))))))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))

  proto/PRecurify
  (recurify [this name arity tail?]
    (let [rator (:rator this)
          rand* (:rand* this)
          RAND* (mapv #(proto/recurify % nil nil false) rand*)]
      (if (and (= rator name) (= (count rand*) arity))
;; NB: seems like a manifest constant
;; NB: maybe fix this with the globals file
          (App. (Simple. 'recur) RAND*)
          (App. (proto/recurify rator nil nil false) RAND*))))
    
  proto/PThunkify
  (thunkify [this]
;; NB: seems like a manifest constant
;; NB: maybe fix this with the globals file
    (let [APP (proto/walk-expr this proto/thunkify nil)]
      (if (= (:rator this) (Simple. 'recur))
          APP
          (Thunk. APP))))

  proto/PUnparse
  (unparse [this]
    `(~(proto/unparse (:rator this)) ~@(map proto/unparse (:rand* this))))

  proto/PUnRecurify
  (unrecurify [this name]
;; NB: seems like a manifest constant
;; NB: maybe fix this with the globals file
    (let [recur-rec (Simple. 'recur)
          unrecurify #(proto/unrecurify % name)]
      (if (= (:rator this) recur-rec)
          (App. recur-rec (mapv unrecurify (:rand* this)))
          (proto/walk-expr this unrecurify nil))))

  proto/PWalkable
  (walk-expr [this f _]
    (App. (f (:rator this)) (mapv f (:rand* this)))))
