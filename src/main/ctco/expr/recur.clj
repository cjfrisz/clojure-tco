;;----------------------------------------------------------------------
;; File recur.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the record type and operations for 'recur' expressions in
;; the TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.recur
  (:require [ctco.protocol :as proto]))

(defrecord Recur [arg*]
  proto/PEmit
    (emit [this]
      (let [arg* (map proto/emit (:arg* this))]
        `(recur ~@arg*))))
