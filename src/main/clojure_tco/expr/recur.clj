;;----------------------------------------------------------------------
;; File recur.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the record type and operations for 'recur' expressions in
;; the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.recur
  (:require [clojure-tco.protocol
             [pemit :as pemit]]))

(defrecord Recur [arg*]
  pemit/PEmit
    (emit [this]
      (let [arg* (map pemit/emit (:arg* this))]
        `(recur ~@arg*))))
