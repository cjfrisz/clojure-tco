;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 29 Aug 2012
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
            Fn FnBody]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.let
            LetCps LetSrs LetTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(declare parse)

(defn- parse-simple
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the parsed representation of the expression if it is a
  simple expression (e.g. number, boolean, nil, etc.). Otherwise, the function
  returns false."
  [expr]
  (and
   ((some-fn nil?
             true?
             false?
             number?
             symbol?
             string?
             keyword?)
    expr)
   (Simple. expr)))

(defn- parse-fn-body
  [fml* cmap bexpr*]
  (FnBody. (vec (map parse fml*)) cmap (vec (map parse bexpr*))))

(defn- parse-fn
  "Helper function for parse that handles 'fn' expressions."
  [body*]
  (Fn. nil (vec (map
                 (fn [b]
                   (condp = (count b)
                     2 (parse-fn-body (first b) nil (next b))
                     3 (parse-fn-body (first b) (fnext b) (nnext b))
                     :else (throw (Exception.
                                   (str "invalid function body" b)))))
                 body*))))

(defn- parse-if
  "Helper function for parse that handles 'if' expressions."
  [test conseq alt]
  (let [TEST (parse test)
        CONSEQ (parse conseq)
        ALT (parse alt)]
    (if (some util/serious? [TEST CONSEQ ALT])
        (IfSrs. TEST CONSEQ ALT)
        (IfTriv. TEST CONSEQ ALT))))

(defn- parse-let
  "Helper function for parse that handles 'let' expressions."
  [bind* body]
  (let [BIND* (vec (map parse bind*))
        BODY (parse body)]
    (assert (even? (count BIND*)))
    (if (or (some util/serious? (take-nth 2 (next BIND*)))
            (util/serious? BODY))
        (LetSrs. BIND* BODY)
        (LetTriv. BIND* BODY))))

(defn- parse-core
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the parsed representation of the expression if it is a core
  Clojure language expression. Otherwise, the function returns false."
  [expr]
  (match [expr]
    [(['fn fml* & bexpr*] :seq)] (parse-fn `((~fml* ~@bexpr*)))
    [(['fn & body*] :seq)] (parse-fn body*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [(['let bind* body] :seq)] (parse-let bind* body)
    :else false))

(defn- parse-defn
  "Helper function for parse that handles 'defn' expression."
  [name func*]
  (letfn [(parse-func* [func* out*]
            (if (nil? (seq func*))
                out*
                (recur (next func*)
                       (conj out* (parse-fn-body (ffirst func*)
                                                 nil
                                                 (nfirst func*))))))]
    (Defn. (Simple. name) (parse-func* func* []))))

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

(defn- parse-composite
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the parsed representation of the expression if it is a
  composite expression. That is, if it is a Clojure expression composed of core
  expressions. If the expression is not a composite expression, the function
  returns false."
  [expr]
  (match [expr]
    [(['defn name (fml* :guard vector?) body] :seq)]
     (parse-defn name `((~fml* ~body)))
    [(['defn name & func*] :seq)] (parse-defn name func*)
    [(['cond & clause*] :seq)] (parse-cond clause*)
    :else false))

(defn- parse-op
  "Helper function for parse that handles simple op expressions (e.g. +, -,
  zero?, nil?, etc."
  [op opnd*]
  (let [OPND* (vec (map parse opnd*))]
    (if (some util/serious? OPND*)
        (SimpleOpSrs. op OPND*)
        (SimpleOpTriv. op OPND*))))

(defn- parse-function-application
  "Helper function for parse that handles function application."
  [rator rand*]
  (App. (parse rator) (vec (map parse rand*))))

(defn- parse-application
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the parsed representation of the expression if it is a form
  of application (e.g. function application, primitive operation a la +, -, *,
  etc.). Otherwise, the function returns false."
  [expr]
  (match [expr]
    [([(op :guard util/simple-op?) & opnd*] :seq)] (parse-op op opnd*)
    [([rator & rand*] :seq)] (parse-function-application rator rand*)
    :else false))

(defn parse
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the expression represented in terms of Clojure TCO
  records."
  [expr]
  (or ((some-fn parse-simple
                parse-core
                parse-composite
                parse-application)
       expr)
      (throw (Exception. (str "Invalid expression in parse: " expr)))))
