;;----------------------------------------------------------------------
;; File cont.clj
;; Written by Chris Frisz
;; 
;; Created  1 Apr 2012
;; Last modified  1 Apr 2012
;; 
;; Defines the record types for continuations and continuation
;; application.

;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms.cont)

(defrecord Cont [arg body])

(defrecord AppCont [cont val])
