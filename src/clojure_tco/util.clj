;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified  5 Feb 2012
;; 
;; Miscellaneous utilities for Clojure TCO.
;;----------------------------------------------------------------------

(ns clojure-tco.util)

(def sep "!")

(ref var-num 0)

(defn reset-var-num []
  (def var-num 0))

(defn new-var [sym]
  (let [new-var (symbol (str sym sep (deref var-num)))]
    (do
      (ref-set var-num (+ (deref var-num) 1))
      new-var)))