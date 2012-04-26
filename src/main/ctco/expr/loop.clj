;;----------------------------------------------------------------------
;; File loop.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the record type and operations for 'loop' expressions in
;; the TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.loop
  (:require [ctco.protocol :as proto]))

(defrecord Loop [bind* body]
  proto/PEmit
    (emit [this]
      (let [bind* (vec (map proto/emit (:bind* this)))
            body (proto/emit (:body this))]
        `(loop ~bind* ~body))))
