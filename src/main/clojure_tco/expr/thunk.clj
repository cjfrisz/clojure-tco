;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  4 Apr 2012
;; 
;; Specifies a record type for thunks in the TCO compiler. Note that
;; thunks are traditionally functions of no arguments
;;----------------------------------------------------------------------

(ns clojure-tco.expr.thunk
  (:require [clojure-tco.protocol
             [pcps :as pcps]]))

(defrecord Thunk [body]
  pcps/PCps
    (triv? [_] true)
    (cps [this] (pcps/cps (:body this)))
    (cps [this k] (pcps/cps (:body this) k)))