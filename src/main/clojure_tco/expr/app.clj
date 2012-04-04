;;----------------------------------------------------------------------
;; File app.clj
;; Written by Chris Frisz
;; 
;; Created  2 Apr 2012
;; Last modified  3 Apr 2012
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
  (walk-expr [this f c]
    ))