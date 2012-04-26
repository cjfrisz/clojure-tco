;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the record type and operations for 'do' expressions in the
;; TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.do
  (:require [ctco.protocol :as proto]))

(defrecord Do [expr*]
  proto/PEmit
    (emit [this]
      (let [expr* (map proto/emit (:expr* this))]
        `(do ~@expr*))))

(defrecord DoSync [expr*]
  proto/PEmit
    (emit [this]
      (let [expr* (map proto/emit (:expr* this))]
        `(dosync ~@expr*))))
