;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified  9 Mar 2012
;; 
;; Defines utilities for trampolining Clojure code. Primarily, this
;; consists of two functions:
;;
;;      1) thunkify, which takes a sequence representing a Clojure
;;	   expression (assumed to be CPSed) and returns it such that
;;	   functions return a function of no arguments (a thunk) whose
;;	   body performs the computation of the original expression
;;
;;	2) tramp, which takes a sequence representing a Clojure
;;         expression (assumed to be CPSed and thunkified and the
;;         symbolic name of the trampoline function and returns the
;;         expression rigged to execute one step at a time via the
;;         named trampoline function.
;;----------------------------------------------------------------------

(ns clojure-tco.tramp
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (reset-var-num new-var triv-op?)]))

(declare
 alpha-rename alpha-rename-fn alpha-rename-if alpha-rename-op alpha-rename-app
 tramp tramp-helper tr-fn tr-if tr-op tr-defn tr-app
 thunkify)

;;-------------------------------------------------------
;; ALPHA-RENAME: Perform alpha-renamine on an expression
;;-------------------------------------------------------
(defn- alpha-rename
  "Performs alpha-renaming from old to new in expr. The expr argument
  is expected to be a sequence representing a Clojure expression.
  Returns expr with the proper renaming done."
  [old new expr]
  (match [expr]
    [(s :when symbol?)] (if (= s old) new s)
    [(:or true false)] expr
    [(n :when number?)] n
    [(['fn fml* body] :seq)] (alpha-rename-fn fml* body old new)
    [(['if test conseq alt] :seq)] (alpha-rename-if test conseq alt old new)
    [([(op :when triv-op?) & opnd*] :seq)] (alpha-rename-op op opnd* old new)
    [([rator & rand*] :seq)] (alpha-rename-app rator rand* old new)
    :else (throw
           (Exception.
            (str "Invalid expression in alpha-rename: " expr)))))

(defn- alpha-rename-fn
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

(defn- alpha-rename-if
  "Helper function for alpha-rename for handling 'if' expressions."
  [test conseq alt old new]
  (let [TEST (alpha-rename old new test)
        CONSEQ (alpha-rename old new conseq)
        ALT (alpha-rename old new alt)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- alpha-rename-op
  "Helper function for alpha-rename for handling simple operators (i.e.
  arithmetic +, -, *, etc.)"
  [op opnd* old new]
  (let [OPND* (map (fn [n] (alpha-rename old new n)) opnd*)]
    `(~op ~@OPND*)))

(defn- alpha-rename-app
  "Helper function for alpha-rename for handling function application."  
  [rator rand* old new]
  (let [RATOR (alpha-rename old new rator)
        RAND* (map (fn [n] (alpha-rename old new n)) rand*)]
    `(~RATOR ~@RAND*)))


;;-------------------------------------------------------
;; TRAMP: Sets up code for trampolining (and helpers)
;;-------------------------------------------------------
(defn tramp
  "Takes a sequence representing a Clojure expression (assumed to be
  CPSed) and returns the trampolined version of the expression. That
  is, it returns the expression such that it executes one step at a
  time."
  ([expr bounce] (tramp expr bounce nil nil))
  ([expr bounce done kv]
     (match [expr]
       [(:or true false)] expr
       [(n :when number?)] n
       [(s :when symbol?)] s
       [(['fn fml* body] :seq)] (tr-fn fml* body bounce done kv)
       [(['if test conseq alt] :seq)] (tr-if test conseq alt bounce done kv)
       [([(op :when triv-op?) & opnd*] :seq)] (tr-op op opnd* bounce done kv)
       [(['defn name fml* body] :seq)] (tr-defn name fml* body bounce done kv)
       [([rator & rand*] :seq)] (tr-app rator rand* bounce done kv)
       :else (throw
              (Exception.
               (str "Invalid expression in tramp: " expr))))))

(defn- tr-fn
  "Helper function for tramp that handles functions."
  [fml* body bounce done kv]
  (if (> (count fml*) 0)
      (let [done (new-var 'done)
            kv (last body)
            fnv (new-var 'fnv)
            thunk (new-var 'th)
            BODY (tramp body bounce done kv)] 
        `(~'fn ~fml*
           (def ~done (~'ref false))
           (let [~fnv (~'fn [fml*] ~BODY)]
             (let [~thunk (~fnv ~@fml*)]
               (~bounce th ~done)))))
      (let [BODY (tramp body bounce done kv)]
        `(~'fn ~fml* ~BODY))))

(defn- tr-if
  "Helper function for tramp that handles 'if' expressions"
  [test conseq alt bounce done kv]
  (let [TEST (tramp test bounce done kv)
        CONSEQ (tramp conseq bounce done kv)
        ALT (tramp alt bounce done kv)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- tr-op
  "Helper function for tramp that handles simple operations (i.e.
  arithmetic +, -, *, etc.)"
  [op opnd* bounce done kv]
  (let [OPND* (map (fn [opnd] (tramp opnd bounce done kv)) opnd*)]
    `(~op ~@OPND*)))

(defn- tr-defn
  "Helper function for tramp that handles 'defn' expressions."
  [name fml* body bounce done kv]
  (let [done (new-var 'done)
        kv (last fml*)
        fnv (new-var name)
        thunk (new-var 'th)
        body-rn (alpha-rename name fnv body)
        BODY-RN (tramp body-rn bounce done kv)]
    `(~'defn ~name
       [~@fml*]
       (def ~done (~'ref false))
       (~'letfn [(~fnv ~fml* ~BODY-RN)]
         (~'let [~thunk  (~fnv ~@fml*)]
           (~bounce ~thunk ~done))))))

(defn- tr-app
  "Helper function for tramp that handles function application."
  [rator rand* bounce done kv]
  (if (= rator kv)
      `(~'do
         (~'dosync (~'ref-set ~done ~'true))
         (~rator ~@rand*))
      (let [RATOR (tramp rator bounce done kv)
            kont (last rand*)
            rand-bl* (butlast rand*)
            RAND-BL* (map
                      (fn [n] (tramp n bounce done kv))
                      rand-bl*)]
        `(~RATOR ~@RAND-BL* ~kont))))


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
    :else (throw
           (Exception. (str "Invalid expression: " expr)))))
