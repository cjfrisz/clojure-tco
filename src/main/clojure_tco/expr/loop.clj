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
  (:require [clojure-tco.expr
             [atomic :as atomic]]
            [clojure-tco.protocol
             [pemit :as pemit]]))

(defrecord Loop [bind* body]
  pemit/PEmit
    (emit [this]
      (let [bind* (vec (map pemit/emit (:bind* this)))
            body (pemit/emit (:body this))]
        `(loop ~bind* ~body))))

(defn make-loop
  "Takes a collection of bindings (assumed to be of even length alternating
  Atomic record types representing variables and arbitrary expression record
  types) and a body expression type and a body expression and returns a Loop
  record with those values."
  [& [bind* body]]
  (let [BIND* (vec bind*)
        BODY (if (nil? body) (atomic/make-atomic 'nil) body)]
    (Loop. BIND* BODY)))
