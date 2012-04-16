;;----------------------------------------------------------------------
;; File defn.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the record type for 'defn' expressions in the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.defn
  (:require [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pemit :as pemit]
             [pcps-triv :as triv]
             [pthunkify :as pthunkify]
             [pwalkable :as pwalkable]]
            [clojure-tco.expr.fn]
            [clojure-tco.expr.thunk])
  (:import [clojure_tco.expr.fn
            Fn]
           [clojure_tco.expr.thunk
            Thunk]))

(defrecord Defn [name func*]
  pabs-k/PAbstractK
    (abstract-k [this app-k]
      (let [f #(pabs-k/abstract-k % app-k)]
        (pwalkable/walk-expr this f nil)))

  pemit/PEmit
    (emit [this]
      (let [name (pemit/emit (:name this))
            fml** (map #(map pemit/emit (:fml* %)) (:func* this))
            body* (map #(pemit/emit (:body %)) (:func* this))
            func* (map #(list (into [] %1) %2) fml** body*)]
        (if (> (count func*) 1)
            `(defn ~name ~@func*)
            (let [FUNC* (apply concat func*)]
              `(defn ~name ~@FUNC*)))))
  
  triv/PCpsTriv
    (cps [this] (pwalkable/walk-expr this triv/cps nil))

  pthunkify/PThunkify
    (thunkify [this] (pwalkable/walk-expr this pthunkify/thunkify nil))

  pwalkable/PWalkable
    (walk-expr [this f _]
      (let [FUNC* (map f (:func* this))]
        (Defn. (:name this) FUNC*))))

(defn make-defn
  "Takes a name (assumed to be an Atomic representing a variable) and one or
  more function definitions (assumed to already be in the appropriate
  representation), and returns a Defn type with those values."
  [name ffunc & rfunc*]
  (let [func* (vec (cons ffunc rfunc*))]
    (Defn. name func*)))
