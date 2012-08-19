;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified  5 Aug 2012
;; 
;; Defines the Do and DoSync record types and operations for 'do' and
;; 'dosync' expressions in the Clojure TCO compiler.
;;
;; Do implements the following protocols:
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(do ~@expr*).
;;
;; DoSync implements the following protocols:
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(dosync ~@expr*).
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
