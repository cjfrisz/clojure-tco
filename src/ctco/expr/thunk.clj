;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  4 Oct 2012
;; 
;; Defines the Thunk record type for representing thunks (functions of
;; zero arguments) in the Clojure TCO compiler. These are used to
;; delimit "bounces" on the trampoline to ensure that the amount of
;; stack memory doesn't become too large.
;;
;; Thunk implements the following protocols:
;;
;;      PAbstractK:
;;              Applies abstract-k to the body expression of the
;;              Thunk.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(with-meta (fn [] ~body) {:thunk true}), using
;;              anonymous 'fn' expressions to represent thunks.
;;----------------------------------------------------------------------

(ns ctco.expr.thunk
  (:require [ctco.protocol :as proto]))

(defrecord Thunk [body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (proto/abstract-k (:body this) app-k)]
        (Thunk. BODY)))

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
