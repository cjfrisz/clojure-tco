;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified 21 Feb 2012
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
