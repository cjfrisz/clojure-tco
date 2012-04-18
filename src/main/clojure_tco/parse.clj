;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 16 Apr 2012
;; 
;; Defines the parser for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.parse
  (:use [clojure.core.match
         :only (match)])
  (:require [clojure-tco.expr
             app atomic defn fn if simple-op]
            [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]])
  (:import [clojure_tco.expr.app
            App]
           [clojure_tco.expr.atomic
            Atomic]
           [clojure_tco.expr.defn
            Defn]
           [clojure_tco.expr.fn
            Fn]
           [clojure_tco.expr.if
            IfCps IfSrs IfTriv]
           [clojure_tco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(declare parse parse-fn parse-defn parse-if parse-op parse-app simple-op?)

(defn parse
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the expression represented in terms of Clojure TCO
  records."
  [expr]
  (match [expr]
    [(:or true false)] (Atomic. expr)
    [(n :when number?)] (Atomic. n)
    [(['quote s] :seq)] (Atomic. `(quote ~s))
    [(v :when symbol?)] (Atomic. v)
    [(['fn fml* body] :seq)] (parse-fn fml* body)
    [(['defn name (fml* :when vector?) body] :seq)] (let [func* `((~fml* ~body))]
                                                      (parse-defn name func*)) 
    [(['defn name & func*] :seq)] (parse-defn name func*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [([(op :when simple-op?) & opnd*] :seq)] (parse-op op opnd*)
    [([rator & rand*] :seq)] (parse-app rator rand*)
    :else (throw (Exception. (str "Invalid expression in parse: " expr)))))

(defn- parse-fn
  "Helper function for parse that handles 'fn' expressions."
  [fml* body]
  (let [FML* (map parse fml*)
        FML* (into [] FML*)
        BODY (parse body)]
    (Fn. FML* BODY)))

(defn- parse-defn
  "Helper function for parse that handles 'defn' expression."
  [name func*]
  (letfn [(parse-func* [func* out*]
            (if (nil? (seq func*))
                out*
                (let [fml* (ffirst func*)
                      body (first (nfirst func*))
                      func (parse-fn fml* body)]
                  (recur (next func*) (conj out* func)))))]
    (let [NAME (Atomic. name)
          FUNC* (parse-func* func* [])]
      (Defn. NAME FUNC*))))

(defn- parse-if
  "Helper function for parse that handles 'if' expressions."
  [test conseq alt]
  (let [TEST (parse test)
        CONSEQ (parse conseq)
        ALT (parse alt)]
    (if (every? #(extends? triv/PCpsTriv (type %)) [TEST CONSEQ ALT])
        (IfTriv. TEST CONSEQ ALT)
        (IfSrs. TEST CONSEQ ALT))))

(defn- parse-op
  "Helper function for parse that handles simple op expressions (e.g. +, -,
  zero?, nil?, etc."
  [op opnd*]
  (let [OPND* (map parse opnd*)]
    (let [OPND* (into [] OPND*)]
      (if (every? #(extends? triv/PCpsTriv (type %)) OPND*)
          (SimpleOpTriv. op OPND*)
          (SimpleOpSrs. op OPND*)))))

(defn- parse-app
  "Helper function for parse that handles application."
  [rator rand*]
  (let [RATOR (parse rator)
        RAND* (map parse rand*)]
    (let [RAND* (into [] RAND*)]
      (App. RATOR RAND*))))

(defn- simple-op?
  "Predicate returning whether op is a simple, value-returning operator."
  [op]
  (some #{op}
        '(+ - * / mod < <= = >= > and or not
          inc dec zero? true? false? nil?
          instance? fn? type ref ref-set deref)))
