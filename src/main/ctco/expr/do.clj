;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 22 Apr 2012
;; 
;; Defines the record type and operations for 'do' expressions in the
;; TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.do
  (:require [ctco.protocol
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
