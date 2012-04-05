;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the record types for function application in the Clojure
;; TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.expr.app
  (:require [clojure-tco.protocol
             [pwalkable :as pwalkable]
             [pcps :as pcps] 
             [pthunkify :as pthunkify]]))

(defrecord App [rator rand*]
  pwalkable/PWalkable
  (walk-expr [this f c] (pwalkable/walk-expr this f c nil))
  (walk-expr [this f c arg*]
    (let [RATOR (apply f (:rator this) arg*)
          RAND* (map #(apply f % arg*) (:rand* this))]
      (c RATOR RAND*)))

  pcps/PCps
  ())
