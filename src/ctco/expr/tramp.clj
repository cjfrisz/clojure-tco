;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris
;; 
;; Created  4 Oct 2012
;; Last modified 20 Oct 2012
;; 
;; Defines the TrampMark record type for marking trampoline entry points
;; and the Tramp record type for marking those entry points with the
;; name of the trampoline function to used in the generated code.
;;
;; Tramp implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the expression as `(~tramp ~expr)
;;
;; TrampMark implements the following protocols:
;;
;;      PLoadTrampoline:
;;              Returns a Tramp record with the given trampoline
;;              function name and the same expression.
;;
;;      PThunkify:
;;              Applies thunkify to the enclosed expression and returns
;;              the result in a new TrampMark record. Uses the walk-expr
;;              function provided by PWalkable.
;;
;;      PWalkable:
;;              Applies the given function to the enclosed expression,
;;              returning the result in a new TrampMark record.
;;----------------------------------------------------------------------

(ns ctco.expr.tramp
  (:require [ctco.protocol :as proto]))

(defrecord Tramp [tramp expr]
  proto/PUnparse
  (unparse [this]
    `(~tramp ~(proto/unparse expr)))

  proto/PWalkable
  (walk-expr [this f _]
    (Tramp. (:tramp this) (f (:expr this)))))

(defrecord TrampMark [expr]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (Tramp. tramp expr))

  proto/PRecurify
  (recurify [this name arity tail?]
    (proto/walk-expr this #(proto/recurify % name arity tail?) nil))
  
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
  (walk-expr [this f _]
    (TrampMark. (f (:expr this)))))
