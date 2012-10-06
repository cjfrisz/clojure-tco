;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the Thunk record type for representing thunks (functions of
;; zero arguments) in the Clojure TCO compiler. These are used to
;; delimit "bounces" on the trampoline to ensure that the amount of
;; stack memory doesn't become too large.
;;
;; Thunk implements the following protocols:
;;
;;      PLoadTrampoline:
;;              Applies load-tramp (with the trampoline function name)
;;              to the body of the thunk, returning a new Thunk
;;              record. Uses the walk-expr function provided by
;;              PWalkable. 
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(with-meta (fn [] ~body) {:thunk true}), using
;;              anonymous 'fn' expressions to represent thunks,
;;              attaching runtime metadata {:thunk true} that is used by
;;              the custom trampoline function to determine whether the
;;              given expression is a thunk or a return value.
;;
;;      PWalkable:
;;              Applies the given function to the body, returning a new
;;              Thunk record with the result.
;;----------------------------------------------------------------------

(ns ctco.expr.thunk
  (:require [ctco.protocol :as proto]))

(defrecord Thunk [body]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) #(Thunk. %)))

  proto/PUnparse
  (unparse [this]
    (let [BODY (proto/unparse (:body this))]
      `(with-meta (fn [] ~BODY) {:thunk true})))

  proto/PWalkable
  (walk-expr [this f _]
    (Thunk. (f (:body this)))))
