;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified  6 Feb 2012
;; 
;; Defines utilities for trampolining Clojure code.
;;----------------------------------------------------------------------

(ns clojure-tco.tramp
  (:use [clojure.algo.monads
         :only (call-cc)]))
