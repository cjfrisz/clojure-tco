;;----------------------------------------------------------------------
;; File new_var.clj
;; Written by Chris Frisz
;; 
;; Created  3 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the new-var function, which returns a new, unique variable
;; for the TCO compiler.
;;----------------------------------------------------------------------

(ns bbc.util.new-var
  (:require [bbc.expr.atomic])
  (:import  [bbc.expr.atomic
             Atomic]))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Atomic. new-var)))
  ([]
     (let [base 'x]
       (new-var base))))
