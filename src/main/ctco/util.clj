;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines miscellaneous utility functions for use in CTCO. These
;; include:
;;      cps-expr
;;      new-var
;;----------------------------------------------------------------------

(ns ctco.util
  (:require [ctco.expr.atomic]
            [ctco.protocol :as proto])
  (:import  [ctco.expr.atomic
             Atomic]))

(declare cps-expr new-var)

(defn cps-expr
  "Applies the appropriate CPS transformation to the given expression according
  to whether it implements PCpsSrs or PCpsTriv."
  ([expr & [k]]
     (condp extends? (type expr)
       proto/PCpsSrs  (proto/cps-srs expr k)
       proto/PCpsTriv (proto/cps-triv expr))))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Atomic. new-var)))
  ([]
     (let [base 'x]
       (new-var base))))
