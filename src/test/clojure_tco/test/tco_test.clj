;;----------------------------------------------------------------------
;; File tco_test.clj
;; Written by Chris Frisz
;; 
;; Created 10 Mar 2012
;; Last modified 25 Mar 2012
;; 
;; Tests for the full tail-call optimization suite.
;;----------------------------------------------------------------------

(ns clojure-tco.test.tco-test
  (:use clojure.test)
  (:use clojure-tco.test.util)
  (:use clojure-tco.cps)
  (:use clojure-tco.abstract-k)
  (:use clojure-tco.thunkify)
  (:use clojure-tco.tco))

(def fact-seq
  '(defn fact
     [n]
     (if (zero? n)
         1
         (* n (fact (dec n))))))

(def ackermann-seq
  '(defn ack
     [x y]
     (if (zero? x)
         (inc y)
         (if (zero? y)
             (ack (dec x) 1)
             (ack (dec x) (ack x (dec y)))))))

(def ackermann-by-hand
  '(defn ack-cps
     [x y k]
     (if (zero? x)
         (k (inc y))
         (if (zero? y)
             (ack-cps (dec x) 1 k)
             (ack-cps x (dec y) (fn [z] (ack-cps (dec x) z k)))))))

(def myeven?-seq
  '(defn myeven?
     [n]
     (if (zero? n) true (myodd? (dec n)))))

(def myodd?-seq
  '(defn myodd?
     [n]
     (if (zero? n) false (myeven? (dec n)))))

(deftest ack-cps-equiv
  (is (alpha-equiv? (cps ackermann-seq) ackermann-by-hand)))
