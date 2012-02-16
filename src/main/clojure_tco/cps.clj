;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified 16 Feb 2012
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
         :only (reset-var-num new-var)]))

(declare simple-op? trivial? E S T)

(defn- simple-op?
  "Returns a boolean whether s is a simple-op"
  [s]
  (let [simple-ops '(+ - * / < <= = >= > zero? inc dec)]
    (some #{s} simple-ops)))

(defn- trivial?
  "Predicate that returns whether a given Clojure expression is
  trivial with respect to the Olivier-style CPS algorithm."
  [t]
  (match [t]
    ;; Booleans
    [(:or true false)] true
    ;; Symbols
    [(s :when symbol?)] true
    ;; Numbers
    [(n :when number?)] true
    ;; Functions
    [(['fn id-ls body] :seq)] true
    ;; If
    [(['if test conseq alt] :seq)]
     (every? trivial? (list test conseq alt))
    ;; Simple ops
    [([(op :when simple-op?) & opnd*] :seq)]
     (every? trivial? opnd*)
    :else                      false))

(defn- E
  "CPS function for an arbitrary Clojure expression with respect to
  the Olivier-style CPS algorithm."
  [expr k]
  (if (trivial? expr)
      (let [EXPR (T expr)]
        `(~k ~EXPR))
      (S expr k)))

(defn- S
  "CPS function for serious Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [expr k]
  (let [S-helper [expr k call* final]
        (if (nil? (seq expr))
            (final call* k)
            (let [fst (first expr)
                  rst (rest expr)]
              (if (trivial? fst)
                  (let [FST (T fst)
                        CALL* `(~@call* ~FST)]
                    (recur rst k CALL* final))
                  (let [s (new-var 's)
                        CALL* `(~@call* ~s)
                        RST (S-helper rst k CALL* final)
                        K `(~'fn [~s] ~RST)]
                    (S fst K)))))]
    (match [expr]
      ;; If
      [(['if test conseq alt] :seq)]
       (let [CONSEQ (E conseq k)
             ALT (E alt k)]
         (if (trivial? test)
             `(if ~test ~CONSEQ ~ALT)
             (let [s (new-var 's)
                   K `(~'fn [~s] (~'if ~s ~CONSEQ ~ALT))]
               (S test K))))
      ;; Simple ops 
      [([(op :when simple-op?) & rand*] :seq)]
       (S-helper rand* k '() (fn [call* k] `(~k (~op ~@call*))))
      ;; Application
      [([rator & rand*] :seq)] 
       (S-helper expr k '() (fn [call* k] `(~@call* ~k))) 
      :else (throw
             (Exception. (str "Invalid serious express: " expr))))))

(defn- T
  "CPS function for trivial Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [expr]
  (match [expr]
    ;; Booleans
    [(:or true false)] expr
    ;; Symbols
    [(s :when symbol?)] s
    ;; Numbers
    [(n :when number?)] n
    ;; Functions
    [(['fn id-ls body] :seq)]
     (let [k (new-var 'k)]
       (let [BODY (E body k)]
         `(~'fn [~@id-ls ~k] ~BODY)))
    ;; If
    [(['if (test :when trivial?)
           (conseq :when trivial?)
           (alt :when trivial?)] :seq)]
     (let [TEST (T test)
           CONSEQ (T conseq)
           ALT (T alt)]
       `(~'if ~TEST ~CONSEQ ~ALT))
    ;; Simple ops
    [([(op :when simple-op?) & opnd*] :seq)]
     (if (every? trivial? opnd*)
         (let [OPND* (map T opnd*)]
           `(~op ~@OPND*))
         (throw
          (Exception.
           (str "Non-trivial simple-op expression in T: " expr))))
    :else (throw
           (Exception. (str "Invalid trivial expression: " expr)))))

(defn cps
  "Entry-point function for the Olivier-style CPS algorithm. Returns a
  sequence representatin a function which takes a Clojure function
  representing a continuation and evaluates the original expression
  with respect to that continuation."
  [expr]
  (do
    (reset-var-num)
    (let [k (new-var 'k)
          EXPR (E expr k)]
      `(~'fn [~k] ~EXPR))))
