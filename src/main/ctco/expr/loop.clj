;;----------------------------------------------------------------------
;; File loop.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Loop record type and operations for representing 'loop'
;; expressions in the Clojure TCO compiler.
;;
;; Loop implements the following protocols:
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(loop ~bind* ~body), where bind* is the vector of
;;              bindings and initial values, and body is the body of
;;              the 'loop' expression.
;;----------------------------------------------------------------------

(ns ctco.expr.loop
  (:require [ctco.protocol :as proto]))

(defrecord Loop [bind* body]
  proto/PEmit
    (emit [this]
      (let [bind* (vec (map proto/emit (:bind* this)))
            body (proto/emit (:body this))]
        `(loop ~bind* ~body))))
