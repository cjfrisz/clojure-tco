;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the record type and operations for 'do' expressions in the
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.do
  (:require [clojure-tco.protocol
             [pemit :as pemit]]))

(defrecord Do [expr*]
  pemit/PEmit
    (emit [this]
      (let [expr* (map pemit/emit (:expr* this))]
        `(do ~@expr*))))

(defrecord DoSync [expr*]
  pemit/PEmit
    (emit [this]
      (let [expr* (map pemit/emit (:expr* this))]
        `(dosync ~@expr*))))

(defn- make-do-type
  "Base function for creating records for all do-type expressions (e.g. do,
  dosync, etc.). Takes ctor for constructing the proper do-type and zero or more
  expressions for the body of the do-type expression and returns a record
  corresponding to ctor."
  [ctor & expr*]
  (let [EXPR* (vec expr*)]
    (ctor EXPR*)))

(defn make-do
  "Takes zero or more expressions (assumed to be in the proper representation)
  and returns a Do record containing those expressions."
  [& expr*]
  (make-do-type ->Do expr*))

(defn make-dosync
  "Takes zero or more expressions (assumed to be in the proper representation)
  and returns a DoSync record containing those expressions."
  [& expr*]
  (make-do-type ->DoSync expr*))
