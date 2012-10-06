;;----------------------------------------------------------------------
;; File if.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the IfSrs, IfTriv, IfCps record types representing serious,
;; trivial, and CPSed 'if' expressions, respectively. IfSrs and IfTriv
;; correspond to 'if' expressions that have subexpressions which are
;; "serious" or "trivial" with respect to the Danvy-style CPS algorithm.
;; IfCps corresponds to an 'if' expression that has undergone the CPS
;; transformation.
;;
;; IfCps implements the following protocols:
;;
;;      PThunkify:
;;              Maps thunkify over the test, consequent, and
;;              alternative of the expression.
;;
;; IfSrs implements the following protocols:
;;
;;      PCpsSrs:
;;              Applies the CPS transformation to the consequent and
;;              alternative of the expression with respect to the
;;              evaluation continuation. If the test is trivial with
;;              respect to the Danvy-style CPS transformation, an
;;              IfCps record is returned with the transformed test,
;;              consequent, and alternative. Otherwise, the test is
;;              pulled out and evaluated first, returning the result
;;              of applying CPS to the test with the 'if' expression
;;              as the continuation.
;;
;;      PThunkify:
;;              Maps thunkify over the test, consequent, and
;;              alternative of the expression.
;;
;; IfTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Maps cps-triv over the test, consequent, and
;;              alternative of the expression.
;;
;;      PThunkify:
;;              Maps thunkify over the test, consequent, and
;;              alternative of the expression.
;;
;;
;; IfCps, IfSrs, and IfTriv use the same implementations for the
;; following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(if ~test ~conseq ~alt)
;;
;;      PWalkable:
;;              Maps the given function over the test, consequent, and
;;              alternative of the expression. 
;;----------------------------------------------------------------------

(ns ctco.expr.if
  (:require [ctco.expr.cont :as cont]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.cont
            Cont AppCont]))

(defrecord IfCps [test conseq alt]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(IfCps. %1 %2 %3)))
  
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(IfCps. %1 %2 %3))))

(defrecord IfSrs [test conseq alt]
  proto/PCpsSrs
  (cps-srs [this k]
    (letfn [(cps [expr]
              (condp extends? (type expr)
                proto/PCpsTriv (let [EXPR (proto/cps-triv expr)]
                                 (AppCont. k EXPR))
                proto/PCpsSrs (proto/cps-srs expr k)))]
      (let [test (:test this)
            CONSEQ (cps (:conseq this))
            ALT (cps (:alt this))]
        (if (extends? proto/PCpsTriv (type test))
            (let [TEST (proto/cps-triv test)]
              (IfCps. TEST CONSEQ ALT))
            (let [s (util/new-var 's)
                  K-body (IfCps. s CONSEQ ALT)
                  K (Cont. s K-body)]
              (proto/cps-srs test K))))))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(IfSrs. %1 %2 %3)))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(IfSrs. %1 %2 %3))))

(defrecord IfTriv [test conseq alt]
  proto/PCpsTriv
  (cps-triv [this]
    (proto/walk-expr this proto/cps-triv #(IfCps. %1 %2 %3)))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(IfTriv. %1 %2 %3)))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(IfTriv. %1 %2 %3))))

(def if-unparse
  {:unparse (fn [this]
              `(if ~(proto/unparse (:test this))
                   ~(proto/unparse (:conseq this))
                   ~(proto/unparse (:alt this))))})

(def if-walkable
  {:walk-expr (fn [this f ctor]
                (ctor (f (:test this)) (f (:conseq this)) (f (:alt this))))})

(util/extend-group (IfCps IfSrs IfTriv)
  proto/PUnparse
  if-unparse

  proto/PWalkable
  if-walkable)
