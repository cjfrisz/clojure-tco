;;----------------------------------------------------------------------
;; File loop.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the record type and operations for 'loop' expressions in
;; the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.loop
  (:require [clojure-tco.protocol
             [pemit :as pemit]]))

(defrecord Loop [bind* body]
  pemit/PEmit
    (emit [this]
      (let [bind* (vec (map pemit/emit (:bind* this)))
            body (pemit/emit (:body this))]
        `(loop ~bind* ~body))))
