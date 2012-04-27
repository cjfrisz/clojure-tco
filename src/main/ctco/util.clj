;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 27 Apr 2012
;; 
;; Defines miscellaneous utility functions for use in CTCO. These
;; include:
;;      cps-expr
;;      new-var
;;----------------------------------------------------------------------

(ns ctco.util
  (:require [ctco.expr
             atomic cont]
            [ctco.protocol :as proto])
  (:import  [ctco.expr.atomic
             Atomic]
            [ctco.expr.cont
             Cont AppCont]))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Atomic. new-var)))
  ([]
     (let [base 'x]
       (new-var base))))
