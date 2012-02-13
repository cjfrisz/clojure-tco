;;----------------------------------------------------------------------
;; File cps_test.clj
;; Written by Christopher Frisz
;; 
;; Created 10 Feb 2012
;; Last modified 13 Feb 2012
;; 
;; Tests for Clojure TCO's CPSer.
;;----------------------------------------------------------------------

(ns clojure-tco.test.cps-test
  (:use [clojure-tco.cps])
  (:use [clojure.test]))

(def empty-k (fn [x] x))

(defn cps-eval-with-k
  [expr k & args]
  (let [cpsed (cps expr)
        evaled (eval cpsed)
        EXPR (evaled k)]
    (if (and (function? evaled) (not (nil? (next args))))
        (apply EXPR args)
        EXPR)))

(defn cps-eval
  [expr & args]
  (apply cps-eval-with-k `(~expr ~empty-k ~@args)))

(deftest id-test-1
  (is (= (cps '(fn [x] x)) '(fn [k!0] (k!0 (fn [x k!1] (k!1 x))))))
  "Make sure that simple tranformations come out right.")

(deftest heavy-redex
  (is (= (cps-eval '(((fn [x] x) (fn [y] y)) (((fn [u] (fn [v] u)) 5) 12)))
         5))
  "Check evaluation of a non-trivial function application")

(deftest heavy-redex-n-arity
  (is (= (cps-eval '(((fn [x] x) (fn [y] y)) ((fn [u v] u) 5 12)))
         5))
  "Check evaluation for non-trivial function application including
  functions with higher arity.")
