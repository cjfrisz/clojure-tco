;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 11 Apr 2012
;; 
;; Defines the parser for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.parse
  (:use [clojure.core.match
         :only (match)])
  (:require [clojure-tco.expr
             app atomic cont defn fn if simple-op]
            [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]])
  (:import [clojure_tco.expr.app
            App]
           [clojure_tco.expr.atomic
            Bool Num Sym Var]
           [clojure_tco.expr.defn
            Defn]
           [clojure_tco.expr.fn
            Fn]
           [clojure_tco.expr.if
            IfCps IfSrs IfTriv]
           [clojure_tco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(declare parse parse-fn parse-if parse-op parse-app simple-op?)

(defn parse
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the expression represented in terms of Clojure TCO
  records."
  [expr]
  (match [expr]
    [(:or true false)] (Bool. expr)
    [(n :when number?)] (Num. n)
    [(['quote s] :seq)] (Sym. s)
    [(v :when symbol?)] (Var. v)
    [(['fn fml* body] :seq)] (parse-fn fml* body)
    [(['defn name fml* body] :seq)] (let [func (parse-fn fml* body)]
                                      (Defn. name func))
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [([(op :when simple-op?) & opnd*] :seq)] (parse-op op opnd*)
    [([rator & rand*] :seq)] (parse-app rator rand*)
    :else (throw (Exception. (str "Invalid expression in parse: " expr)))))

(defn- parse-fn
  "Helper function for parse that handles 'fn' expression."
  [fml* body]
  (let [FML* (map parse fml*)
        BODY (parse body)]
    (Fn. FML* BODY)))

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
    (if (every? #(extends? triv/PCpsTriv (type %)) OPND*)
        (SimpleOpTriv. op OPND*)
        (SimpleOpSrs. op OPND*))))

(defn- parse-app
  "Helper function for parse that handles application."
  [rator rand*]
  (let [RATOR (parse rator)
        RAND* (map parse rand*)]
    (App. RATOR RAND*)))

(defn- simple-op?
  [op]
  (some #{op} '(+ - * / mod zero? true? false? nil?)))
