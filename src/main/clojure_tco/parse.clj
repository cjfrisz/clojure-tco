;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz 
;; 
;; Created 26 Mar 2012
;; Last modified 27 Mar 2012
;; 
;; Front-end parser and verifier for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.parse
  (:require [clojure.core.match :as core.match
             :only (match)])
  (:require [clojure-tco.lang-forms])
  (:import [clojure_tco.lang_forms
            Bool Num Var TrivOp If Fn Defn App])
  (:require [clojure-tco.util :as util
             :only (triv-op?)]))

(declare
 parse parse-fn parse-defn parse-if parse-cond parse-op parse-app parse-let
 unsupported)

(defmacro parse
  "Parses an expression and verifies that it's supported in the Clojure TCO compiler. If so,
  returns the record representation of the expression, otherwise raises an exception."
  [expr]
  (core.match/match [expr]
    [(:or true false)] (Bool. expr)
    [(n :when number?)] (Num. n)
    [(s :when symbol?)] (Var. s)
    [([(op :when util/triv-op?) & opnd*] :seq)] (parse-op op opnd*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [(['cond & clause*] :seq)] (parse-cond clause*)
    [(['fn fml* body] :seq)] (parse-fn fml* body)
    [(['defn name fml* body] :seq)] (parse-defn name fml* body)
    [(['let bind* body] :seq)] (parse-let bind* body)
    [([rator & rand*] :seq)] (parse-app rator rand*)
    :else (throw (Exception. (str "Unsupported expression: " expr)))))

(defn- parse-fn
  "Helper function for parse that parses 'fn' expressions."
  [fml* body]
  (let [BODY (macroexpand `(parse ~body))]
    (Fn. fml* BODY)))

(defn- parse-defn
  "Helper function for parse that parses 'defn' expression."
  [name fml* body]
  (let [BODY (macroexpand `(parse ~body))]
    (Defn. name fml* BODY)))

(defn- parse-if
  "Helper function for parse that parses 'if' expressions."
  [test conseq alt]
  (let [TEST (macroexpand `(parse ~test))
        CONSEQ (macroexpand `(parse ~conseq))
        ALT (macroexpand `(parse ~alt))]
    (If. TEST CONSEQ ALT)))

(defn- parse-cond
  "Helper function for parse that parses 'cond' expressions.
  N.B. 'cond' expressions are not yet supported."
  [clause*]
  (unsupported 'cond))

(defn- parse-op
  "Helper function for parse that parses operands of expressions using simple
  operators (i.e. aritmetic and relational)."
  [op opnd*]
  (let [OPND* (for [x opnd*] (macroexpand `(parse ~x)))]
    (TrivOp. op OPND*)))

(defn- parse-let
  "Helper function for parse that parses 'let' expression.
  N.B. 'let' is not yet supported."
  [bind* body]
  (unsupported 'let))

(defn- parse-app
  "Helper function for parse that parses the operator and operands of
  function application."
  [rator rand*]
  (let [RATOR (macroexpand `(parse ~rator))
        RAND* (for [x rand*] (macroexpand `(parse ~x)))]
    (App. RATOR RAND*)))

(defn- unsupported
  "Helper function acting as a catch-all for unsupported language forms in
  Clojure TCO."
  [expr]
  (throw (Exception. (str expr " expressions not supported in Clojure TCO."))))