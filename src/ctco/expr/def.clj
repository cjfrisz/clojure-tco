;;----------------------------------------------------------------------
;; File def.clj
;; Written by Chris
;; 
;; Created 30 Aug 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the DefSrs, DefTriv, and DefCps record types for representing
;; 'def' expression in the Clojure TCO compiler.
;;
;; DefCps implements the following protocols:
;;
;;      PLoadTrampoline:
;;              Applies load-tramp to the init expression for the given
;;              trampoline function name, generating a new DefCps with
;;              the same symbol name. Uses the walk-expr function
;;              provided by PWalkable.
;;
;;      PThunkify:
;;              Applies thunkify to the init expression, generating a
;;              new DefCps with the same symbol name. Uses the walk-expr
;;              function provided by PWalkable.
;;
;; DefSrs implements the following protocols:
;;
;;      PCpsSrs:
;;              Applies cps-srs to the init expression, generating a new
;;              DefCps with the same symbol name. Uses the walk-expr
;;              function provided by PWalkable.
;;
;;      PLoadTrampoline:
;;              Applies load-tramp to the init expression for the given
;;              trampoline function name, generating a new DefSrs with
;;              the same symbol name. Uses the walk-expr function
;;              provided by PWalkable.
;;
;; DefTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Applies cps-srs to the init expression, generating a new
;;              DefCps with the same symbol name. Uses the walk-expr
;;              function provided by PWalkable.
;;
;;      PLoadTrampoline:
;;              Applies load-tramp to the init expression for the given
;;              trampoline function name, generating a new DefTriv with
;;              the same symbol name. Uses the walk-expr function
;;              provided by PWalkable.
;;
;; DefCps, DefSrs, and DefTriv have the same implementations for the
;; following protocols:
;;
;;      PUnparse:
;;              Returns a sequence representing the expression as an
;;              s-expression, recursively unparsing the symbol and init
;;              value. I.e., `(def ~(unparse sym) ~(unparse init))
;;
;;      PWalkable:
;;              Applies the given function to the init expression,
;;              returning a new DefCps, DefSrs, or DefTriv with the same
;;              symbol name depending on the given constructor.
;;----------------------------------------------------------------------

(ns ctco.expr.def
  (:require [ctco.protocol :as proto]
            [ctco.util :as util]))

(defrecord DefCps [sym init]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(DefCps. %1 %2)))
  
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify #(DefCps. %1 %2))))

(defrecord DefSrs [sym init]
  proto/PCpsSrs
  (cps-srs [this k]
    (proto/walk-expr this #(proto/cps-srs % k) #(DefCps. %1 %2)))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(DefSrs. %1 %2))))

(defrecord DefTriv [sym init]
  proto/PCpsTriv
  (cps-triv [this]
    (proto/walk-expr this proto/cps-triv #(DefCps. %1 %2)))

  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(DefTriv. %1 %2))))

(util/extend-multi (DefCps DefSrs DefTriv)
  proto/PUnparse
  (unparse [this]
    `(def ~(proto/unparse (:sym this))
       ~(proto/unparse (:init this))))
 
  proto/PWalkable
  (walk-expr [this f ctor]
    (ctor (:sym this) (f (:init this)))))
