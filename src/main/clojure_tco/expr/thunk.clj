;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Specifies a record type for thunks in the TCO compiler. Note that
;; thunks are traditionally functions of no arguments
;;----------------------------------------------------------------------

(ns clojure-tco.expr.thunk
  (:require [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pemit :as pemit]]))

(defrecord Thunk [body]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (pabs-k/abstract-k (:body this) app-k)]
        (Thunk. BODY)))

  pemit/PEmit
    (emit [this]
      (let [BODY (pemit/emit (:body this))]
        `(fn [] ~BODY))))

(defn make-thunk
  "Takes a body expression and returns a Thunk record with that body value."
  [body]
  (Thunk. body))
