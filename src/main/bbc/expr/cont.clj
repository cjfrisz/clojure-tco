;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the record types for continuations and continuation
;; application.
;;----------------------------------------------------------------------

(ns bbc.expr.cont
  (:require [bbc.protocol
             [pabstract-k :as pabs-k]
             [pemit :as pemit]
             [pthunkify :as pthunkify]]))

(defrecord Cont [arg body]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (let [BODY (pabs-k/abstract-k (:body this) app-k)]
        (Cont. (:arg this) BODY)))
  
  pemit/PEmit
    (emit [this]
      (let [arg (pemit/emit (:arg this))
            body (pemit/emit (:body this))]
        `(fn [~arg] ~body))))

(defrecord AppContAbs [app-k cont val]
  pemit/PEmit
    (emit [this]
      (let [app-k (pemit/emit (:app-k this))
            cont (pemit/emit (:cont this))
            val (pemit/emit (:val this))]
        `(~app-k ~cont ~val))))

(defrecord AppCont [cont val]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (let [CONT (pabs-k/abstract-k (:cont this) app-k)
            VAL (pabs-k/abstract-k (:val this) app-k)]
        (AppContAbs. app-k CONT VAL)))

  pemit/PEmit
    (emit [this]
      (let [cont (pemit/emit (:cont this))
            val (pemit/emit (:val this))]
        `(~cont ~val))))

(def cont-thunkify
  {:thunkify identity})

(extend Cont
  pthunkify/PThunkify
    cont-thunkify)

(extend AppContAbs
  pthunkify/PThunkify
    cont-thunkify)

(extend AppCont
  pthunkify/PThunkify
    cont-thunkify)
