;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Specifies a record type for thunks in the TCO compiler. Note that
;; thunks are traditionally functions of no arguments
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
