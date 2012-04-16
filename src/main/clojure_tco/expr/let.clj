;;----------------------------------------------------------------------
;; File let.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the record type for 'let' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.let
  (:require [clojure-tco.expr
             [atomic :as atomic]]
            [clojure-tco.protocol
             [pemit :as pemit]]))

(defrecord Let [bind* body]
  pemit/PEmit
    (emit [this]
      (let [bind* (vec (map pemit/emit (:bind* this)))
            body (pemit/emit (:body this))]
        `(let ~bind* ~body))))

(defn make-let
  "Possibly takes a collection (assumed to be even in length alternating Atomic
  records corresponding to variables and arbitrary expression records) and a
  body expression and returns a Let record with those values.

  Note that this may also take zero arguments corresponding to Clojure's
  semantics that 'let' requires neither bindings nor a body expression."
  [& [bind* body]]
  (let [BIND* (vec bind*)
        BODY (if (nil? body) (atomic/make-atomic 'nil) body)]
    (Let. BIND* BODY)))
