;;----------------------------------------------------------------------
;; File tco_test.clj
;; Written by Christopher Frisz
;; 
;; Created 10 Mar 2012
;; Last modified 10 Mar 2012
;; 
;; Tests for the full tail-call optimization suite.
;;----------------------------------------------------------------------

(ns clojure-tco.test.tco-test
  (:use clojure.test)
  (:require [clojurecheck.core :as cc])
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