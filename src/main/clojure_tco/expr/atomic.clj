;;----------------------------------------------------------------------
;; File atomic.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  2 Apr 2012
;; 
;; Implements the PExpr protocol functions for atomic expressions
;; (e.g. booleans, integers, symbols, etc.).
;;----------------------------------------------------------------------

(ns clojure-tco.expr.atomic
  (:require [clojure-tco.protocol
             [pwalkable :as pwalkable]
             [pcps :as pcps] 
             [pthunkify :as pthunkify]]))

(defrecord Atomic [val]
  pwalkable/PWalkable
    (walk-expr [this f c]
      (let [THIS (f this)]
        (c (:val THIS))))
    (walk-expr [this f c args]
      (let [THIS (apply f this args)]
        (c (:val THIS))))

  pcps/PCps
    (triv? [_] true)
    (cps [this] this)
    (cps [this _] this)

  pthunkify/PThunkify
    (thunkify [this] this))
