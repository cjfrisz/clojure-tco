;;----------------------------------------------------------------------
;; File thunkify.clj
;; Written by Chris Frisz
;; 
;; Created 21 Mar 2012
;; Last modified 21 Mar 2012
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
    [(['fn fml* body] :seq)] `(~'fn ~fml* (~'fn [] ~(thunkify body)))
    [(['if test conseq alt] :seq)]   (let [TEST (thunkify test)
                                           CONSEQ (thunkify conseq)
                                           ALT (thunkify alt)]
                                       `(~'if ~TEST ~CONSEQ ~ALT))
    [([(op :when triv-op?) & opnd*] :seq)] (let [OPND* (map thunkify opnd*)]
                                             `(~op ~@OPND*))
    [([(:or 'defn 'defn-) name fml* body] :seq)] (let [deftype (first expr)
                                                       BODY (thunkify body)]
                                                   `(~deftype ~name [~@fml*]
                                                      (~'fn [] ~BODY)))
    [([rator & rand*] :seq)] (let [rand-bl* (butlast rand*)
                                   k (last rand*)
                                   RATOR (thunkify rator)
                                   RAND-BL* (map thunkify rand-bl*)]
                               `(~RATOR ~@RAND-BL* ~k))
    :else (throw (Exception. (str "Invalid expression: " expr)))))