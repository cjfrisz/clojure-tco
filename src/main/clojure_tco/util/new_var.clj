;;----------------------------------------------------------------------
;; File new_var.clj
;; Written by Chris Frisz
;; 
;; Created  3 Apr 2012
;; Last modified  4 Apr 2012
;; 
;; Defines the new-var function, which returns a new, unique variable
;; for the TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.util.new-var
  (:require [clojure-tco.expr.atomic])
  (:import  [clojure_tco.expr.atomic Var]))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Var. new-var)))
  ([]
     (let [base 'x]
       (new-var base))))
