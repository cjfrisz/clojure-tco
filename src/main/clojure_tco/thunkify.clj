;;----------------------------------------------------------------------
;; File thunkify.clj
;; Written by Chris Frisz
;; 
;; Created 21 Mar 2012
;; Last modified 22 Mar 2012
;; 
;; Primarily defines the function "thunkify," which takes a sequence
;; representing a Clojure expression (assumed to be CPSed) and returns
;; it such that functions return a function of no arguments (a thunk)
;; whose body performs the computation of the original expression
;;----------------------------------------------------------------------

(ns clojure-tco.thunkify
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (triv-op?)]))

(declare
 thunkify thunkify-fn thunkify-if thunkify-op thunkify-defn thunkify-app)

;;---------------------------------------------
;; THUNKIFY: Makes all functions return thunks
;;---------------------------------------------
(defn thunkify
  "Takes a sequence representing a Clojure expression, assumed to be
  in CPS, and returns the expression such that any function returns a
  function of no arguments (called a thunk). Invoking the thunk
  either returns the value as it would have been produced by the
  original expression or another thunk. Any returned thunks can be
  invoked in turn until a value is produced. This can be seen as
  performing the computation in steps and is useful in conjunction
  with trampolining."
  [expr]
  (match [expr]
    [(:or true false)] expr
    [(n :when number?)] n
    [(s :when symbol?)] s
    [(['fn fml* body] :seq)] (thunkify-fn fml* body)
    [(['if test conseq alt] :seq)] (thunkify-if test conseq alt)
    [([(op :when triv-op?) & opnd*] :seq)] (thunkify-op op opnd*)
    [(['defn name fml* body] :seq)] (thunkify-defn name fml* body)
    [([rator & rand*] :seq)] (thunkify-app rator rand*)
    :else (throw (Exception. (str "Invalid expression: " expr)))))

(defn- thunkify-fn
  "Helper function for thunkify that handles 'fn' expressions."
  [fml* body]
  (let [BODY (thunkify body)]
    `(~'fn ~fml* (~'fn [] ~BODY))))

(defn- thunkify-if
  "Helper function for thunkify that handles 'if' expressions."
  [test conseq alt]
  (let [TEST (thunkify test)
        CONSEQ (thunkify conseq)
        ALT (thunkify alt)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- thunkify-op
  "Helper function for thunkify that handles simple operators (i.e. arithmetic,
  relational, etc.)"
  [op opnd*]
   (let [OPND* (map thunkify opnd*)]
     `(~op ~@OPND*)))

(defn- thunkify-defn
  "Helper function for thunkify that handles 'defn' expressions."
  [name fml* body]
  (let [BODY (thunkify body)]
    `(~'defn ~name ~fml*
       (~'fn [] ~BODY))))

(defn- thunkify-app
  "Helper function for thunkify that handles function application."
  [rator rand*]
  (let [rand-bl* (butlast rand*)
        k (last rand*)
        RATOR (thunkify rator)
        RAND-BL* (map thunkify rand-bl*)]
    `(~RATOR ~@RAND-BL* ~k)))