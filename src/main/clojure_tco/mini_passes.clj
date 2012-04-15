;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified 15 Apr 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-flag
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns clojure-tco.mini-passes)