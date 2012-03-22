;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified 22 Mar 2012
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
         :only (reset-var-num new-var triv-op?)]))

(declare
 cps
 expr
 srs srs-if srs-op srs-app
 triv triv-fn triv-if triv-op triv?
 abstract-k abstract-k-main abstract-k-fn abstract-k-if abstract-k-op
 abstract-k-op abstract-k-defn abstract-k-app)

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
      [(['defn name fml* body] :seq)] (let [BODY (expr body k)]
                                        `(~'defn ~name [~@fml* ~k] ~BODY))
      :else                           (let [E (expr e k)]
                                        `(~'fn [~k] ~E)))))

;;----------------------------------------
;; EXPR: General expression CPS function
;;----------------------------------------
(defn- expr
  "CPS function for an arbitrary Clojure expression with respect to
  the Olivier-style CPS algorithm."
  [e k]
  (match [e]
    [(['defn name fml* body] :seq)] (cps e)
    :else                           (if (triv? e)
                                        `(~k ~(triv e))
                                        (srs e k))))


;;--------------------------------------------------
;; SRS: Serious expression CPS function (and helpers
;;--------------------------------------------------
(defn- srs
  "CPS function for serious Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [e k]
  (match [e]
    [(['if test conseq alt] :seq)]         (srs-if test conseq alt k)
    [([(op :when triv-op?) & rand*] :seq)] (let [final (fn [arg* k]
                                                           `(~k (~op ~@arg*)))]
                                               (srs-app rand* k '() final))
    [([rator & rand*] :seq)]               (let [final (fn [arg* k]
                                                         `(~@arg* ~k))]
                                             (srs-app e k '() final)) 
    :else (throw
           (Exception.
            (str "Invalid serious expression in srs: " e)))))

(defn- srs-if
  "Helper function for srs that handles non-trivial 'if' expressions"
  [test conseq alt k]
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
            (let [FST (triv fst)
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
    [(:or true false)]                     e
    [(s :when symbol?)]                    s
    [(n :when number?)]                    n
    [(['fn fml* body] :seq)]               (triv-fn fml* body)
    [(['if test conseq alt] :seq)]         (triv-if test conseq alt)
    [([(op :when triv-op?) & opnd*] :seq)] (triv-op op opnd*)
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
    [(:or true false)]                     true
    [(s :when symbol?)]                    true
    [(n :when number?)]                    true
    [(['fn fml* body] :seq)]               true
    [(['if test conseq alt] :seq)]         (and
                                            (triv? test)
                                            (triv? conseq)
                                            (triv? alt))
    [([(op :when triv-op?) & opnd*] :seq)] (every? triv? opnd*)
    :else false))

;;--------------------------------------------------
;; ABSRACT-K: Abstracts continuation application
;;--------------------------------------------------
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
    [(:or true false)] expr
    [(n :when number?)] n
    [(s :when symbol?)] s
    [(['fn fml* body] :seq)] (abstract-k-fn fml* body app-k)
    [(['if test conseq alt] :seq)] (abstract-k-if test conseq alt app-k kv)
    [([(op :when triv-op?) & opnd*] :seq)] (abstract-k-op op opnd* app-k kv)
    [(['defn name fml* body] :seq)] (abstract-k-defn name fml* body app-k)
    [([rator & rand*] :seq)] (abstract-k-app rator rand* app-k kv)
    :else (throw (Exception. (str "Invalid expression: " e)))))

(defn- abstract-k-fn
  "Helper function for abstract-k-main that handles anonymous functions."
  [fml* body app-k]
  (let [kv (last fml*)
        BODY (abstract-k-main body app-k kv)]
    `(~'fn ~fml* ~BODY)))

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
    (~op ~@OPND*)))

(defn- abstract-k-defn
  "Helper function for abstract-k-main that handles 'defn' expressions."
  [name fml* body app-k]
  (let [kv (last fml*)
        BODY (abstract-k-main body app-k kv)]
    `(~'defn ~fml* ~BODY)))

(defn- abstract-k-app
  "Helper function fo abstract-k-app that handles function application."
  [rator rand* app-k kv]
  (let [RATOR (abstract-k-main rator app-k kv)
        RAND* (map (fn [x] (abstract-k-main x app-k kv)) rand*)]
    (if (= RATOR kv)
        `(~app-k ~RATOR ~@RAND*)
        `(~RATOR ~@RAND*))))