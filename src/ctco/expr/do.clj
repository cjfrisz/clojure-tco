;;----------------------------------------------------------------------
;; File do.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 30 Aug 2012
;; 
;; Defines the Do and DoSync record types and operations for 'do' and
;; 'dosync' expressions in the Clojure TCO compiler.
;;
;; Do implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(do ~@expr*).
;;
;; DoSync implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(dosync ~@expr*).
;;----------------------------------------------------------------------

(ns ctco.expr.do
  (:require [ctco.protocol :as proto]))

(defrecord Do [expr*]
  proto/PUnparse
    (unparse [this]
      (let [expr* (map proto/unparse (:expr* this))]
        `(do ~@expr*))))

(defrecord DoSync [expr*]
  proto/PUnparse
    (unparse [this]
      (let [expr* (map proto/unparse (:expr* this))]
        `(dosync ~@expr*))))
