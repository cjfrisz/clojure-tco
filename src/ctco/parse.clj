;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 26 Aug 2012
;; 
;; Defines the parser for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.parse
  (:use [clojure.core.match
         :only (match)])
  (:require [ctco.expr
             app simple defn fn if let simple-op]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.app
            App]
           [ctco.expr.simple
            Simple]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            FnBody]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.let
            LetCps LetSrs LetTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(declare parse parse-fn parse-defn parse-if parse-cond parse-let parse-op
         parse-app)

(defn parse
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the expression represented in terms of Clojure TCO
  records."
  [expr]
  (match [expr]
    [nil] (Simple. nil)
    [(:or true false)] (Simple. expr)
    [(n :guard number?)] (Simple. n)
    [(['quote s] :seq)] (Simple. `(quote ~s))
    [(v :guard symbol?)] (Simple. v)
    [(s :guard string?)] (Simple. s)
    [(k :guard keyword?)] (Simple. k)
    [(['fn fml* body] :seq)] (parse-fn fml* body)
    [(['defn name (fml* :guard vector?) body] :seq)] (parse-defn name
                                                       `((~fml* ~body)))
    [(['defn name & func*] :seq)] (parse-defn name func*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [(['cond & clause*] :seq)] (parse-cond clause*)
    [(['let bind* body] :seq)] (parse-let bind* body)
    [([(op :guard util/simple-op?) & opnd*] :seq)] (parse-op op opnd*)
    [([rator & rand*] :seq)] (parse-app rator rand*)
    :else (throw (Exception. (str "Invalid expression in parse: " expr)))))

(defn- parse-fn
  "Helper function for parse that handles 'fn' expressions."
  [fml* body]
  (FnBody. (vec (map parse fml*)) (parse body)))

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
    (let [NAME (Simple. name)
          FUNC* (parse-func* func* [])]
      (Defn. NAME FUNC*))))

(defn- parse-if
  "Helper function for parse that handles 'if' expressions."
  [test conseq alt]
  (let [TEST (parse test)
        CONSEQ (parse conseq)
        ALT (parse alt)]
    (if (some util/serious? [TEST CONSEQ ALT])
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
                      RST (if (some util/serious? [test conseq rst])
                              (IfSrs. test conseq rst)
                              (IfTriv. test conseq rst))
                      RCLAUSE* (nnext rclause*)]
                  (recur RCLAUSE* RST))))]
    (parse-rclause* (reverse clause*) (Simple. nil))))

(defn- parse-let
  "Helper function for parse that handles 'let' expressions."
  [bind* body]
  (let [BIND* (vec (map parse bind*))
        BODY (parse body)]
    ;; Technically only need check inits, but no point in separating them
    (if (or (some util/serious? BIND*) (util/serious? BODY))
        (LetSrs. BIND* BODY)
        (LetTriv. BIND* BODY))))

(defn- parse-op
  "Helper function for parse that handles simple op expressions (e.g. +, -,
  zero?, nil?, etc."
  [op opnd*]
  (let [OPND* (vec (map parse opnd*))]
    (if (some util/serious? OPND*)
        (SimpleOpSrs. op OPND*)
        (SimpleOpTriv. op OPND*))))

(defn- parse-app
  "Helper function for parse that handles application."
  [rator rand*]
  (App. (parse rator) (vec (map parse rand*))))
    

