;;----------------------------------------------------------------------
;; File parse.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 19 Oct 2012
;; 
;; Defines the parser for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco.parse
  (:use [clojure.core.match
         :only (match)])
  (:require [ctco.expr
             app def fn if let recur simple simple-op]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.app
            App]
           [ctco.expr.def
            DefSrs DefTriv]
           [ctco.expr.fn
            Fn FnBody]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.let
            LetCps LetSrs LetTriv]
           [ctco.expr.recur
            Recur]
           [ctco.expr.simple
            Simple]
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

(defn- parse-def
  "Helper function for parsing 'def' expressions."
  [sym init]
  (let [SYM (parse sym)
        INIT (parse init)]
    (condp extends? (type INIT)
      proto/PCpsTriv (DefTriv. SYM INIT)
      proto/PCpsSrs  (DefSrs. SYM INIT)
      :else (throw (Exception. (str "unexpected expression in def " init))))))

(defn- parse-fn-body
  [fml* cmap bexpr*]
  (FnBody. (mapv parse fml*) cmap (mapv parse bexpr*)))

(defn- parse-fn
  "Helper function for parse that handles 'fn' expressions."
  [name body*]
  (Fn.
   (parse (or name (gensym "fn")))
   (mapv
    #(match [%]
       [([fml* (cmap :guard map?) & b*] :seq)] (parse-fn-body fml* cmap b*)
       [([fml* & b*] :seq)] (parse-fn-body fml* nil b*)
       :else (throw (Exception. (str "invalid function body" %))))
    body*)))

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
  (let [BIND* (mapv parse bind*)
        BODY (parse body)]
    (assert (even? (count BIND*)))
    (if (or (some util/serious? (take-nth 2 (next BIND*)))
            (util/serious? BODY))
        (LetSrs. BIND* BODY)
        (LetTriv. BIND* BODY))))

(defn- parse-recur
  "Helper function for parse that handles 'recur' expressions."
  [expr*]
  (Recur. (mapv parse expr*)))

(defn- parse-core
  "Takes a sequence representing a Clojure expression (generally passed from a
  macro) and returns the parsed representation of the expression if it is a core
  Clojure language expression. Otherwise, the function returns false."
  [expr]
  (match [expr]
    [(['def sym] :seq)] (parse-def sym nil)
    [(['def sym init] :seq)] (parse-def sym init)
    [(['fn (fml* :guard vector?) & bexpr*] :seq)] (parse-fn nil `((~fml* ~@bexpr*)))
    [(['fn (name :guard symbol?)
       (fml* :guard vector?) & bexpr*] :seq)] (parse-fn name `((~fml* ~@bexpr*)))
    [(['fn & body*] :seq)] (parse-fn nil body*)
    [(['fn (name :guard symbol?) & body*] :seq)] (parse-fn name body*)
    [(['if test conseq alt] :seq)] (parse-if test conseq alt)
    [(['let bind* body] :seq)] (parse-let bind* body)
    [(['recur & expr*] :seq)] (parse-recur expr*)
    :else false))

(defn- parse-defn
  "Helper function for parse that handles 'defn' expressions. Currently
  translates (defn name body*) into (def name (fn body*))"
  [name func*]
  (DefTriv. (parse name) (parse-fn name func*)))

(defn- parse-cond
  "Helper function for parse that handles 'cond' expressions. Currently
  parses it in terms of 'if' expressions."
  [clause*]
  (loop [rclause* (reverse clause*)
         rst (Simple. nil)]
    (if (nil? (seq rclause*))
        rst
        (let [conseq (parse (first rclause*))
              test (parse (fnext rclause*))]
          (recur (nnext rclause*)
                 (if (some util/serious? [test conseq rst])
                     (IfSrs. test conseq rst)
                     (IfTriv. test conseq rst)))))))

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
  (let [OPND* (mapv parse opnd*)]
    (if (some util/serious? OPND*)
        (SimpleOpSrs. op OPND*)
        (SimpleOpTriv. op OPND*))))

(defn- parse-function-application
  "Helper function for parse that handles function application."
  [rator rand*]
  (App. (parse rator) (mapv parse rand*)))

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
