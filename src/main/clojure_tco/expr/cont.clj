;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified 11 Apr 2012
;; 
;; Defines the record types for continuations and continuation
;; application.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.cont
  (:require [clojure-tco.protocol
             [pemit :as pemit]
             [pthunkify :as pthunkify]]))

(defrecord Cont [arg body]
  pemit/PEmit
    (emit [this]
      (let [arg (pemit/emit (:arg this))
            body (pemit/emit (:body this))]
        `(fn [~arg] ~body))))

(defrecord AppCont [cont val])

(def cont-thunkify {:thunkufy identity})

(extend Cont
  pthunkify/PThunkify
  cont-thunkify)

(extend AppCont
  pthunkify/PThunkify
  cont-thunkify)
