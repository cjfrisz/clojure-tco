;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified 13 Mar 2012
;; 
;; Miscellaneous utilities for Clojure TCO.
;;
;; Defines the "new-var" function and associated helpers which
;; generate semi-unique identifiers with a given symbol root
;; symbol. This acts as a prettier gensym for performing the
;; algorithms like CPS.
;;----------------------------------------------------------------------

(ns clojure-tco.util
  (:use [clojure.core.match
         :only (match)]))

(declare
 sep var-num reset-var-num new-var triv-op?
 alpha-rename ar-fn ar-if ar-op ar-app)

(def
  ^{:private true}
  sep
  "The separator which goes between the root symbol and number for
  identifiers."
  "!")

(def
  ^{:private true}
  var-num
  "The var-num which makes each new variable semi-unique."
  (atom 0))

(defn reset-var-num
  "Resets the var-num value to zero."
  []
  (swap! var-num (fn [x] 0)))

(defn new-var
  "Takes a symbol and returns an identifier with the given symbol as
  the root followed by a semi-unique suffix."
  [sym]
  (let [new-var (symbol (str sym sep @var-num))]
    (do
      (swap! var-num inc)
      new-var)))

(defn triv-op?
  "Returns a boolean whether s is a simple-op"
  [s]
  (let [simple-ops '(+ - * / < <= = >= > zero? inc dec)]
    (some #{s} simple-ops)))

;;-------------------------------------------------------
;; ALPHA-RENAME: Perform alpha-renaming on an expression
;;-------------------------------------------------------
(defn alpha-rename
  "Performs alpha-renaming from old to new in expr. The expr argument
  is expected to be a sequence representing a Clojure expression.
  Returns expr with the proper renaming done."
  [old new expr]
  (match [expr]
    [(s :when symbol?)] (if (= s old) new s)
    [(:or true false)] expr
    [(n :when number?)] n
    [(['fn fml* body] :seq)] (ar-fn fml* body old new)
    [(['if test conseq alt] :seq)] (ar-if test conseq alt old new)
    [([(op :when triv-op?) & opnd*] :seq)] (ar-op op opnd* old new)
    [([rator & rand*] :seq)] (ar-app rator rand* old new)
    :else (throw
           (Exception.
            (str "Invalid expression in alpha-rename: " expr)))))

(defn- ar-fn
  "Helper function for alpha-rename for handling functions."
  [fml* body old new]
  (cond
    (some #{old} fml*) `(~'fn ~fml* ~body)
    (some #{new} fml*) (let [alt (new-var new)
                             FML* (replace {new alt} fml*)
                             body-alt (alpha-rename new alt body)
                             BODY-ALT (alpha-rename old new body)]
                         `(~'fn ~FML* ~BODY-ALT))
    :else              (let [BODY (alpha-rename old new body)]
                         `(~'fn ~fml* ~BODY))))

(defn- ar-if
  "Helper function for alpha-rename for handling 'if' expressions."
  [test conseq alt old new]
  (let [TEST (alpha-rename old new test)
        CONSEQ (alpha-rename old new conseq)
        ALT (alpha-rename old new alt)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- ar-op
  "Helper function for alpha-rename for handling simple operators (i.e.
  arithmetic +, -, *, etc.)"
  [op opnd* old new]
  (let [OPND* (map (fn [n] (alpha-rename old new n)) opnd*)]
    `(~op ~@OPND*)))

(defn- ar-app
  "Helper function for alpha-rename for handling function application."  
  [rator rand* old new]
  (let [RATOR (alpha-rename old new rator)
        RAND* (map (fn [n] (alpha-rename old new n)) rand*)]
    `(~RATOR ~@RAND*)))
