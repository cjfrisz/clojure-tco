;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 28 Apr 2012
;; 
;; Defines miscellaneous utility functions for use in CTCO. These
;; include:
;;      new-var
;;      simple-op?
;;      serious?
;;      trivial?
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

(defn simple-op?
  "Predicate returning whether op is a simple, value-returning operator."
  [op]
  (some #{op}
        '(+ - * / mod < <= = >= > and or not
          inc dec zero? true? false? nil?
          instance? fn? type ref ref-set deref)))

(defn serious?
  "Given a Clojure TCO expression (as a record type), returns whether the
  expression is serious with respect to the Danvy FOOP CPS transformation."
  [expr]
  (extends? proto/PCpsSrs (type expr)))

(defn trivial?
  "Given a Clojure TCO expression (as a record type), returns whether the
  expression is trivial with respect to the Danvy FOOP CPS transformation."
  [expr]
  (extends? proto/PCpsTriv (type expr)))
