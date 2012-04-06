;;----------------------------------------------------------------------
;; File thunk.clj
;; Written by Chris Frisz
;; 
;; Created  4 Apr 2012
;; Last modified  5 Apr 2012
;; 
;; Specifies a record type for thunks in the TCO compiler. Note that
;; thunks are traditionally functions of no arguments
;;----------------------------------------------------------------------

(ns clojure-tco.expr.thunk)

(defrecord Thunk [body])