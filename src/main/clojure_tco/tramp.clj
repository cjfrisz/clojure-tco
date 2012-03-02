;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified  1 Mar 2012
;; 
;; Defines utilities for trampolining Clojure code.
;;----------------------------------------------------------------------

(ns clojure-tco.tramp
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (reset-var-num new-var)]))

(defn- simple-op?
  "Returns a boolean whether s is a simple-op"
  [s]
  (let [simple-ops '(+ - * / < <= = >= > zero? inc dec)]
    (some #{s} simple-ops)))

(defn- simple?
  "Returns a boolean as to whether the given expression is simple"
  [s]
  (loop [pred* [true? false? symbol? number?]]
    (and (seq? pred*) (or ((first pred*) s) (recur (rest pred*))))))

(defn tramp
  "Takes a sequence representing a Clojure expression (assumed to be
  CPSed) and returns the trampolined version of the expression. That
  is, it returns the expression such that it executes one step at a
  time."
  [expr]
  (let [tramp-helper
        (fn [expr k]
          (match [expr]
            [(s :when simple?)] (k s)
            [(['fn fml* body] :seq)]
            ;; We assume that the whole body of code will undergo
            ;; this tranformation, so we also trampoline the body of
            ;; the anonymous fn
            (let [BODY (tramp body)]
              (k `(~'fn ~fml* ~BODY)))
            [(['if test conseq alt] :seq)]
            ;; The test isn't a value-producing context, so we *shouldn't
            ;; have to traverse it further. 
            (let [CONSEQ (tramp-helper conseq k)
                  ALT (tramp-helper alt k)]
              `(if ~test ~CONSEQ ~ALT))
            ;; Operands to a simple operation are not value producing
            [([(op :when simple-op?) & opnd*] :seq)]
            (let [OPND* (map
                         (fn [opnd] (tramp-helper opnd (fn [x] x)))
                         opnd*)]
              (k `(~op ~OPND*)))
            [([(:or 'defn 'defn-) name fml* body] :seq)]
            ))]))

(defn thunkify
  "Returns the expression in which functions return thunks"
  [expr]
  (match [expr]
    [(:or true false)] expr
    [(s :when symbol?)] s
    [(n :when number?)] n
    [(['fn fml* body] :seq)]
      (let [BODY (thunkify body)]
        `(~'fn ~fml* (~'fn [] ~BODY)))
    [(['if test conseq alt] :seq)]
      (let [TEST (thunkify test)
            CONSEQ (thunkify conseq)
            ALT (thunkify alt)]
        `(~'if ~TEST ~CONSEQ ~ALT))
    [([(op :when simple-op?) & opnd*] :seq)]
      (let [OPND* (map thunkify opnd*)]
        `(~op ~@OPND*))
    [([(:or 'defn 'defn-) name fml* body] :seq)]
      (let [deftype (first expr)
            BODY (thunkify body)]
        `(~deftype ~name ~fml* (~'fn [] ~BODY)))
    [([rator rand] :seq)]
      (let [RATOR (thunkify rator)
            RAND (thunkify rand)]
        `(~RATOR ~RAND))
    :else (throw
           (Exception. (str "Invalid expression: " expr)))))
