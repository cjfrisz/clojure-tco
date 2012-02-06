;;----------------------------------------------------------------------
;; File test-runner.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified  6 Feb 2012
;; 
;; Defines macros for testing the correctness of clojure-tco
;; tranformations and checking the executation time and memory usage
;; for transformed programs versus umodified ones.
;;----------------------------------------------------------------------

(ns clojure-tco.test.test-runner
  (:use [clojure-tco.test.test-suite]))
