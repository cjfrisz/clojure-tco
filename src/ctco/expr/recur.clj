;;----------------------------------------------------------------------
;; File recur.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified  6 Oct 2012
;; 
;; Defines the Recur record type and operations for representing
;; 'recur' expressions in the Clojure TCO compiler.
;;
;; Recur implements the following protocols:
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(loop ~bind* ~body), where bind* is the vector of
;;              variables and bindings, and body is the body
;;              expression of the 'loop.'
;;----------------------------------------------------------------------

(ns ctco.expr.recur
  (:require [ctco.protocol :as proto]))

(defrecord Recur [arg*]
  proto/PUnparse
  (unparse [this]
    (let [arg* (map proto/unparse (:arg* this))]
      `(recur ~@arg*))))
