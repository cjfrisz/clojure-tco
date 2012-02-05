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

(def var-num (atom 0))

(defn reset-var-num []
  (swap! var-num (fn [x] 0)))

(defn new-var [sym]
  (let [new-var (symbol (str sym sep @var-num))]
    (do
      (swap! var-num inc)
      new-var)))