;;----------------------------------------------------------------------
;; File abstract_k.clj
;; Written by Christopher Frisz
;; 
;; Created 22 Mar 2012
;; Last modified 22 Mar 2012
;; 
;; Defines the abstract-k function which takes a sequence representing
;; a Clojure expression and a symbol representing the name of a
;; function for applying continuations and abstracts over continuation
;; application using the function.
;;----------------------------------------------------------------------

(ns clojure-tco.abstract-k
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (triv-op?)]))

(declare
 abstract-k abstract-k-main abstract-k-fn abstract-k-if abstract-k-op
 abstract-k-op abstract-k-defn abstract-k-app abstract-k-kont)

(defn abstract-k
  "Takes a sequence representing a CPSed Clojure expression and a
  symbol representing a handler for applying continuation and returns
  the expression such that it is representationally independent with
  respect to continuations using the given handler for applying
  continuations."
  [e app-k]
  (abstract-k-main e app-k nil))

(defn- abstract-k-main
  "Helper function for abstract-k that additionally carries the name
  of the continuation argument."
  [e app-k kv]
  (match [e]
    [(:or true false)] e
    [(n :when number?)] n
    [(s :when symbol?)] s
    [(['fn fml* body] :seq)] (abstract-k-fn fml* body app-k kv)
    [(['if test conseq alt] :seq)] (abstract-k-if test conseq alt app-k kv)
    [([(op :when triv-op?) & opnd*] :seq)] (abstract-k-op op opnd* app-k kv)
    [(['defn name fml* body] :seq)] (abstract-k-defn name fml* body app-k kv)
    [([rator & rand*] :seq)] (abstract-k-app rator rand* app-k kv)
    :else (throw (Exception. (str "Invalid expression: " e)))))

(defn- abstract-k-fn
  "Helper function for abstract-k-main that handles anonymous functions."
  [fml* body app-k kv]
  (let [body-abs (abstract-k-main body app-k kv)
        ka (last fml*)
        BODY-ABS (abstract-k-main body app-k ka)]
    `(~'fn ~fml* ~BODY-ABS)))

(defn- abstract-k-if
  "Helper function for abstract-k-main that handles 'if' expressions."
  [test conseq alt app-k kv]
  (let [TEST (abstract-k-main test app-k kv)
        CONSEQ (abstract-k-main conseq app-k kv)
        ALT (abstract-k-main alt app-k kv)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- abstract-k-op
  "Helper function for abstract-k-main that handles simple operators (i.e.
  arithmetic, relational, etc.)"
  [op opnd* app-k kv]
  (let [OPND* (map (fn [x] (abstract-k-main x app-k kv)) opnd*)]
    `(~op ~@OPND*)))

(defn- abstract-k-defn
  "Helper function for abstract-k-main that handles 'defn' expressions."
  [name fml* body app-k kv]
  (let [body-abs (abstract-k-main body app-k kv)
        ka (last fml*)
        BODY-ABS (abstract-k-main body app-k ka)]
    `(~'defn ~fml* ~BODY-ABS)))

(defn- abstract-k-app
  "Helper function for abstract-k-main that handles function application."
  [rator rand* app-k kv]
  (let [rand-bl* (butlast rand*)
        RAND-BL* (map (fn [x] (abstract-k-main x app-k kv)) rand-bl*)
        kd (last rand*)]
    (if (= rator kv)
        `(~app-k ~rator ~@RAND-BL* ~kd) ; RAND-BL* should be empty here
        (let [RATOR (abstract-k-main rator app-k kv)
              KD (abstract-k-kont kd app-k kv)]
          `(~RATOR ~@RAND-BL* ~KD)))))

(defn- abstract-k-kont
  "Helper function for abstract-k-app that handles continuation arguments."
  [k app-k kv]
  (match [k]
    [(s :when symbol?)]      s
    [(['fn fml* body] :seq)] (let [BODY (abstract-k-main body app-k kv)]
                                 `(~'fn ~fml* ~BODY))
    :else (throw (Exception. (str "Invalid continuation: " k)))))
