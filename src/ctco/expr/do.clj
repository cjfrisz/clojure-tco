;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 18 Oct 2012
;; 
;; Defines the Do record type and operations for 'do' expressions in the
;; Clojure TCO compiler.
;;
;; Do implements the following protocols:
;;
;;      PLoadTrampoline:
;;              Applies load-tramp to each expression using the given
;;              trampoline function name. Uses the walk-expr function
;;              provided by PWalkable.
;;
;;      PThunkify:
;;              Applies thunkify to each expression. Uses the walk-expr
;;              function provided by PWalkable.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(do ~@expr*).
;;
;;      PUnRecurify:
;;              Applies unrecurify to each expression. Uses the
;;              walk-expr function provided by PWalkable.
;;
;;      PWalkable:
;;              Applies the given function to each expression, creating
;;              a new Do record with the results.
;;----------------------------------------------------------------------

(ns ctco.expr.do
  (:require [ctco.protocol :as proto]))

(defrecord Do [expr*]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))

  proto/PThunkify
  (thunkify [this] (proto/walk-expr this proto/thunkify nil))
  
  proto/PUnparse
  (unparse [this] `(do ~@(map proto/unparse (:expr* this))))

  proto/PUnRecurify
  (unrecurify [this name]
    (proto/walk-expr this #(proto/unrecurify % name) nil))

  proto/PWalkable
  (walk-expr [this f _] (Do. (mapv f (:expr* this)))))
