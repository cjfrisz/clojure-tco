;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 27 Apr 2012
;; 
;; Defines miscellaneous utility functions for use in CTCO. These
;; include:
;;      new-var
;;      some-srs?
;;----------------------------------------------------------------------

(ns ctco.util
  (:require [ctco.expr
             atomic cont]
            [ctco.protocol :as proto])
  (:import  [ctco.expr.atomic
             Atomic]
            [ctco.expr.cont
             Cont AppCont]))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Atomic. new-var)))
  ([]
     (let [base 'x]
       (new-var base))))

(defn some-srs?
  "Given a collection of Clojure TCO record expressions, returns whether one of
  them is serious with respect to the Danvy-style CPS algorithm. That is, if at
  least one of them extends the PCpsSrs protocol."
  [expr*]
  (some #(extends? proto/PCpsSrs (type %)) expr*))
