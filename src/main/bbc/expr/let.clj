;;----------------------------------------------------------------------
;; File let.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the record type for 'let' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns bbc.expr.let
  (:require [bbc.protocol
             [pemit :as pemit]]))

(defrecord Let [bind* body]
  pemit/PEmit
    (emit [this]
      (let [bind* (vec (map pemit/emit (:bind* this)))
            body (pemit/emit (:body this))]
        `(let ~bind* ~body))))
