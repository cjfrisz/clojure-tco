;;----------------------------------------------------------------------
;; File cps-test.clj
;; Written by Christopher Frisz
;; 
;; Created 10 Feb 2012
;; Last modified 10 Feb 2012
;; 
;; Tests for Clojure TCO's CPSer.
;;----------------------------------------------------------------------

(ns clojure-tco.test.cps-test
  (:use [clojure-tco.cps])
  (:use [clojure.test]))

(deftest id-test-1
  (is (= (cps '(fn [x] x)) '(fn [k!0] (k!0 (fn [x k!1] (k!1 x)))))))