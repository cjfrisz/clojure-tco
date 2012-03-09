;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified  9 Mar 2012
;; 
;; Defines CPS algorithm for Clojure expressions. The "cps" function
;; takes a sequence representing a Clojure expression and returns
;; another sequence reqresenting the CPS version of that expression
;; which, when evaluated, takes a Clojure function representing a
;; continuation and evaluates the original expression with respect to
;; that continuation.
;;
;; The algorithm currently supports the following kinds of
;; expressions:
;;      Booleans
;;      Symbols
;;      Numbers
;;      Functions using the "fn" form
;;      Function applications involving aforementioned supported
;;            functions.
;;      "If" expressions
;;      Simple arithmetic and relational operators:
;;              +, -, *, /, <, <=, =, >=, >, zero, inc, dec
;;
;; This implementation is intended for use as part of the Clojure TCO
;; project for implementing tail-call optimization in Clojure.
;;
;; The CPS algorithm defined is based on one presented in "A
;; First-Order One Pass CPS Algorithm" by Danvy, 2001.
;;----------------------------------------------------------------------

(ns clojure-tco.cps
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (reset-var-num new-var simple-op?)]))

(declare cps expr srs srs-if srs-op srs-app triv triv-fn triv-if triv-op triv?)

;;------------------------------
;; CPS: Entry-point function
;;------------------------------
(defn cps
  "Entry-point function for the Olivier-style CPS algorithm. Returns a
  sequence representatin a function which takes a Clojure function
  representing a continuation and evaluates the original expression
  with respect to that continuation."
  [e]
  (let [k (new-var 'k)]
    (match [e]
      [(['defn name fml* body] :seq)]
      (let [BODY (expr body k)]
        `(~'defn ~name [~@fml* ~k] ~BODY))
      :else
      (let [E (expr e k)]
        `(~'fn [~k] ~E)))))

;;----------------------------------------
;; EXPR: General expression CPS function
;;----------------------------------------
(defn- expr
  "CPS function for an arbitrary Clojure expression with respect to
  the Olivier-style CPS algorithm."
  [e k]
  (if (triv? e)
      (let [E (triv e)]
        `(~k ~E))
      (srs e k)))


;;--------------------------------------------------
;; SRS: Serious expression CPS function (and helpers
;;--------------------------------------------------
(defn- srs
  "CPS function for serious Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [e k]
  (match [e]
    [(['if test conseq alt] :seq)] (srs-if test conseq alt)
    [([(op :when simple-op?) & rand*] :seq)] (let [final (fn [arg* k]
                                                           `(~k (~op ~@arg*)))]
                                               (srs-app rand* k '() final))
    [([rator & rand*] :seq)] (let [final (fn [arg* k] `(~@arg* ~k))]
                               (srs-app e k '() final)) 
    :else (throw
           (Exception.
            (str "Invalid serious expression in srs: " e)))))

(defn- srs-if
  "Helper function for srs that handles non-trivial 'if' expressions"
  [test conseq alt]
  (let [CONSEQ (expr conseq k)
        ALT (expr alt k)]
    (if (triv? test)
        `(if ~test ~CONSEQ ~ALT)
        (let [s (new-var 's)
              K `(~'fn [~s] (~'if ~s ~CONSEQ ~ALT))]
          (srs test K)))))

(defn- srs-op
  "Helper function for srs that handles simple operators
  (i.e. arithmetic +, -, *, etc.)"
  [op rand* k]
  (let [arg* '()
        final (fn [arg* k] `(~k (~op ~@arg*)))]
    (srs-app rand* k final)))

(defn- srs-app
  "Helper function for srs that handles function applications."
  [e k arg* final]
  (if (nil? (seq e))
      (final arg* k)
      (let [fst (first e)
            rst (rest e)]
        (if (triv? fst)
            (let [FST (T fst)
                  ARG* `(~@arg* ~FST)]
              (recur rst k ARG* final))
            (let [s (new-var 's)
                  ARG* `(~@arg* ~s)
                  RST (srs-app rst k ARG* final)
                  K `(~'fn [~s] ~RST)]
              (srs fst K))))))

;;-------------------------------------------------------
;; TRIV: Trivial expression CPS function (and helpers)
;;-------------------------------------------------------
(defn- triv
  "CPS function for trivial Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [e]
  (match [e]
    [(:or true false)] e
    [(s :when symbol?)] s
    [(n :when number?)] n
    [(['fn fml* body] :seq)] (triv-fn fml* body)
    [(['if test conseq alt] :seq)] (triv-if test conseq alt)
    [([(op :when simple-op?) & opnd*] :seq)] (triv-op op opnd*)
    [([(:or 'defn 'defn-) name fml* body] :seq)] ;; Without the docstring
     (let [deftype (first e)
           k (new-var 'k)
           BODY (expr body k)]
       `(~deftype ~name [~@fml* ~k] ~BODY))     
    [([(:or 'defn 'defn-) name doc fml* body] :seq)]
     (let [deftype (first e)
           k (new-var 'k)
           BODY (expr body k)]
       `(~deftype ~name ~(str doc) [~@fml* ~k] ~BODY))
    :else (throw
           (Exception. (str "Invalid trivial expression: " e)))))

(defn- triv-fn
  "Helper function for 'triv' that handles function definitions."
  [fml* body]
  (let [k (new-var 'k)
        BODY (expr body k)]
    `(~'fn [~@fml* ~k] ~BODY)))

(defn- triv-if
  "Helper function for 'triv' that handles 'if' expressions"
  [test conseq alt]
  (if (and (triv? test) (triv? conseq) (triv? alt))
      (let [TEST (triv test)
            CONSEQ (triv conseq)
            ALT (triv alt)]
        `(~'if ~TEST ~CONSEQ ~ALT))
      (throw
       (Exception.
        (str "Non-trivial 'if' in trivial context: "
             `(if ~test ~conseq ~alt))))))

(defn- triv-op
  [op opnd*]
  (if (every? triv? opnd*)
      (let [OPND* (map triv opnd*)]
        `(~op ~@OPND*))
      (throw
       (Exception.
        (str "Non-trivial simple-op expression in triv: " `(~op ~@opnd*))))))

(defn- triv?
  "Predicate that returns whether a given Clojure expression is
  trivial with respect to the Olivier-style CPS algorithm."
  [t]
  (match [t]
    [(:or true false)] true
    [(s :when symbol?)] true
    [(n :when number?)] true
    [(['fn fml* body] :seq)] true
    [(['if test conseq alt] :seq)] (every? triv? (list test conseq alt))
    [([(op :when simple-op?) & opnd*] :seq)] (every? triv? opnd*)
    :else false))