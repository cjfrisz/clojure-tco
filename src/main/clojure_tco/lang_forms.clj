;;----------------------------------------------------------------------
;; File lang_forms.clj
;; Written by Chris Frisz
;; 
;; Created 26 Mar 2012
;; Last modified 26 Mar 2012
;; 
;; Defines the records for the language forms in Clojure TCO.
;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms)

(defrecord Boolean [val])

(defrecord Number [val])

(defrecord Var [val])

(defrecord TrivOp [op opnd*])

(defrecord If [test conseq alt])

(defrecord Cond [clause*])

(defrecord Fn [fml* body])

(defrecord Defn [name fml* body])

(defrecord Let [bind* body])