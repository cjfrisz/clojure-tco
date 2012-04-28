;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 27 Apr 2012
;; 
;; Defines the parser for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.parse
  (:use [clojure.core.match
         :only (match)])
  (:require [ctco.expr
             app atomic defn fn if simple-op]
            [ctco.protocol :as proto])
  (:import [ctco.expr.app
            App]
           [ctco.expr.atomic
            Atomic]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            Fn]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(declare parse parse-fn parse-defn parse-if parse-cond parse-op parse-app
         simple-op?)

(defn parse
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the expression represented in terms of Clojure TCO
  records."
  [expr]
  (match [expr]
    [nil] (Atomic. nil)
    [(:or true false)] (Atomic. expr)
    [(n :when number?)] (Atomic. n)
    [(['quote s] :seq)] (Atomic. `(quote ~s))
    [(v :when symbol?)] (Atomic. v)
    [(k :when keyword?)] (Atomic. k)
    [(['fn fml* body] :seq)] (parse-fn fml* body)
    [(['defn name (fml* :when vector?) body] :seq)] (let [func* `((~fml* ~body))]
                                                      (parse-defn name func*)) 
    [(['defn name & func*] :seq)] (parse-defn name func*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [(['cond & clause*] :seq)] (parse-cond clause*)
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
    (if (util/some-srs? [TEST CONSEQ ALT])
        (IfSrs. TEST CONSEQ ALT)
        (IfTriv. TEST CONSEQ ALT))))

(defn- parse-cond
  "Helper function for parse that handles 'cond' expressions. Currently
  parses it in terms of 'if' expressions."
  [clause*]
  (letfn [(parse-rclause* [rclause* rst]
            (if (nil? (seq rclause*))
                rst
                (let [conseq (parse (first rclause*))
                      test (parse (fnext rclause*))
                      RST (if (util/some-srs? [test conseq rst])
                              (IfSrs. test conseq rst)
                              (IfTriv. test conseq rst))
                      RCLAUSE* (nnext rclause*)]
                  (recur RCLAUSE* RST))))]
    (parse-rclause* (reverse clause*) (Atomic. nil))))

(defn- parse-op
  "Helper function for parse that handles simple op expressions (e.g. +, -,
  zero?, nil?, etc."
  [op opnd*]
  (let [OPND* (vec (map parse opnd*))]
    (if (util/some-srs? OPND*)
        (SimpleOpSrs. op OPND*)
        (SimpleOpTriv. op OPND*))))

(defn- parse-app
  "Helper function for parse that handles application."
  [rator rand*]
  (let [RATOR (parse rator)
        RAND* (vec (map parse rand*))]
    (App. RATOR RAND*)))

(defn- simple-op?
  "Predicate returning whether op is a simple, value-returning operator."
  [op]
  (some #{op}
        '(+ - * / mod < <= = >= > and or not
          inc dec zero? true? false? nil?
          instance? fn? type ref ref-set deref)))
