;;----------------------------------------------------------------------
;; File let.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Let record type for representing 'let' expressions in
;; the Clojure TCO compiler.
;;
;; Let implements the following protocols:
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(let ~bind* ~body), where bind* is the vector of
;;              variables and bindings, and body is the body
;;              expression of the 'let.'
;;----------------------------------------------------------------------

(ns ctco.expr.let
  (:require [ctco.protocol :as proto]))

(defrecord Let [bind* body]
  proto/PEmit
    (emit [this]
      (let [bind* (vec (map proto/emit (:bind* this)))
            body (proto/emit (:body this))]
        `(let ~bind* ~body))))
