;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified 26 Apr 2012
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
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(fn [] ~body), using anonymous 'fn' expressions to
;;              represent thunks.
;;----------------------------------------------------------------------

(ns ctco.expr.thunk
  (:require [ctco.protocol :as proto]))

(defrecord Thunk [body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (proto/abstract-k (:body this) app-k)]
        (Thunk. BODY)))

  proto/PEmit
    (emit [this]
      (let [BODY (proto/emit (:body this))]
        `(fn [] ~BODY))))
