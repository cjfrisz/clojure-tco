;;----------------------------------------------------------------------
;; File defn.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the Defn record type for 'defn' expressions in the TCO
;; compiler. It supports multiple arity definitions and represents
;; them as a vector of Fn records.
;;
;; Defn implements the following protocols:
;;
;;      PAbstractK:
;;              Maps abstract-k over the enclosed function
;;              definitions.
;;
;;      PEmit:
;;              Emits (recursively) the expression as syntax i the
;;              form `(defn ~name ~@body*) where body* is the list of
;;              vector of emitted function definitions only including
;;              the formal parameters lists and bodies.
;;
;;      PCpsTriv:
;;              Maps cps-triv over the vector of function definitions.
;;
;;      PThunkify:
;;              Maps thunkify over the vector of function definitions.
;;
;;      PWalkable:
;;              Maps the given function over the vector of function
;;              definitions, returning a new Defn record.
;;----------------------------------------------------------------------

(ns ctco.expr.defn
  (:require [ctco.expr
             fn thunk]
            [ctco.protocol :as proto])
  (:import [ctco.expr.fn
            Fn]
           [ctco.expr.thunk
            Thunk]))

(defrecord Defn [name func*]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [f #(proto/abstract-k % app-k)]
        (proto/walk-expr this f nil)))

  proto/PEmit
    (emit [this]
      (let [name (proto/emit (:name this))
            fml** (map #(map proto/emit (:fml* %)) (:func* this))
            body* (map #(proto/emit (:body %)) (:func* this))
            func* (map #(list (into [] %1) %2) fml** body*)]
        (if (> (count func*) 1)
            `(defn ~name ~@func*)
            (let [FUNC* (apply concat func*)]
              `(defn ~name ~@FUNC*)))))
  
  proto/PCpsTriv
    (cps-triv [this] (proto/walk-expr this proto/cps-triv nil))

  proto/PThunkify
    (thunkify [this] (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
    (walk-expr [this f _]
      (let [FUNC* (vec (map f (:func* this)))]
        (Defn. (:name this) FUNC*))))
