;;----------------------------------------------------------------------
;; File let.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the record type for 'let' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.let
  (:require [ctco.protocol :as proto]))

(defrecord Let [bind* body]
  proto/PEmit
    (emit [this]
      (let [bind* (vec (map proto/emit (:bind* this)))
            body (proto/emit (:body this))]
        `(let ~bind* ~body))))
