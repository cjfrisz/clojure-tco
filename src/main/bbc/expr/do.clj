;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the record type and operations for 'do' expressions in the
;; TCO compiler.
;;----------------------------------------------------------------------

(ns bbc.expr.do
  (:require [bbc.protocol
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
