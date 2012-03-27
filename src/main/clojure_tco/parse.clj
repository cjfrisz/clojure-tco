;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz 
;; 
;; Created 26 Mar 2012
;; Last modified 26 Mar 2012
;; 
;; Front-end parser and verifier for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.parse
  (:require [clojure.core.match :as core.match
             :only (match)])
  (:require [clojure-tco.util :as util
             :only (triv-op?)]))

(declare
 verify verify-fn verify-if verify-cond verify-op verify-app verify-let
 unsupported)

(defmacro verify
  "Verifies that an expression is supported in the Clojure TCO compiler. If so,
  returns true, otherwise raises an exception."
  [expr]
  (core.match/match [expr]
    [(:or true false)] true
    [(n :when number?)] true
    [(s :when symbol?)] true
    [([(op :when util/triv-op?) & opnd*] :seq)] (verify-op opnd*)
    [(['if test conseq alt] :seq)] (verify-if test conseq alt)
    [(['cond & clause*] :seq)] (verify-cond clause*)
    [(['fn fml* body] :seq)] (verify-fn fml* body)
    [(['defn name fml* body] :seq)] (verify-fn fml* body)
    [(['let bind* body] :seq)] (verify-let bind* body)
    [([rator & rand*] :seq)] (verify-app rator rand*)
    :else (throw (Exception. (str "Unsupported expression: " expr)))))

(defn- verify-fn
  "Helper function for verify that verifies 'fn' and 'defn' expressions."
  [fml* body]
  (do
    (apply distinct? fml*)
    (verify body)))

(defn- verify-if
  "Helper function for verify that verifies 'if' expressions."
  [test conseq alt]
  (do
    (verify test)
    (verify conseq)
    (verify alt)))

(defn- verify-cond
  "Helper function for verify that verifies 'cond' expressions.
  N.B. 'cond' expressions are not yet supported."
  [clause*]
  (unsupported 'cond))

(defn- verify-op
  "Helper function for verify that verifies operands of expressions using simple
  operators (i.e. aritmetic and relational)."
  [opnd*]
  (for [x opnd*] (verify x)))

(defn- verify-let
  "Helper function for verify that verifies 'let' expression.
  N.B. 'let' is not yet supported."
  [bind* body]
  (unsupported 'let))

(defn- verify-app
  "Helper function for verify that verifies the operator and operands of
  function application."
  [rator rand*]
  (do
    (verify rator)
    (for [x rand*] (verify x))))

(defn- unsupported
  "Helper function acting as a catch-all for unsupported language forms in
  Clojure TCO."
  [expr]
  (throw (Exception. (str expr " expressions not supported in Clojure TCO."))))