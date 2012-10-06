;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris
;; 
;; Created  4 Oct 2012
;; Last modified  5 Oct 2012
;; 
;; Defines the TrampEntry record for denoting trampoline entry points in
;; CTCO.
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
  
  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
  (walk-expr [this f _]
    (TrampMark. (f (:expr this)))))
