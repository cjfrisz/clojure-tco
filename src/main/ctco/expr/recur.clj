;;----------------------------------------------------------------------
;; File recur.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 22 Apr 2012
;; 
;; Defines the record type and operations for 'recur' expressions in
;; the TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.expr.recur
  (:require [ctco.protocol
             [pemit :as pemit]]))

(defrecord Recur [arg*]
  pemit/PEmit
    (emit [this]
      (let [arg* (map pemit/emit (:arg* this))]
        `(recur ~@arg*))))
