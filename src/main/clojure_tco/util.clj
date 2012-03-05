;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified  5 Mar 2012
;; 
;; Miscellaneous utilities for Clojure TCO.
;;
;; Defines the "new-var" function and associated helpers which
;; generate semi-unique identifiers with a given symbol root
;; symbol. This acts as a prettier gensym for performing the
;; algorithms like CPS.
;; ----------------------------------------------------------------------

(ns clojure-tco.util)

;; The separator which goes between the root symbol and number for
;; identifiers.
(def sep "!")

;; The var-num which makes each new variable semi-unique.
(def var-num (atom 0))

;; Resets the var-num value to zero.
(defn reset-var-num []
  (swap! var-num (fn [x] 0)))

(defn new-var
  "Takes a symbol and returns an identifier with the given symbol as
  the root followed by a semi-unique suffix."
  [sym]
  (let [new-var (symbol (str sym sep @var-num))]
    (do
      (swap! var-num inc)
      new-var)))

(defn- simple-op?
  "Returns a boolean whether s is a simple-op"
  [s]
  (let [simple-ops '(+ - * / < <= = >= > zero? inc dec)]
    (some #{s} simple-ops)))
