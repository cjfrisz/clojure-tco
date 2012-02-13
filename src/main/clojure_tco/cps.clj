;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified 13 Feb 2012
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

(declare trivial? E S T)

(defn trivial?
  "Predicate that returns whether a given Clojure expression is
  trivial with respect to the Olivier-style CPS algorithm."
  [t]
  (match [t]
    [(b :when [true? false?])] true
    [(s :when symbol?)]        true
    [(n :when number?)]        true
    [(['fn id-ls body] :seq)]  true
    :else                      false))

(defn E
  "CPS function for an arbitrary Clojure expression with respect to
  the Olivier-style CPS algorithm."
  [expr k]
  (if (trivial? expr)
      (let [EXPR (T expr)]
        `(~k ~EXPR))
      (S expr k)))

(defn S
  "CPS function for serious Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [expr k]
  (defn S-app [expr k call]
    (if (nil? (seq expr))
        `(~@call ~k)
        (let [fst (first expr)
              rst (rest expr)]
          (if (trivial? fst)
              (let [FST (T fst)
                    CALL `(~@call ~FST)]
                (recur rst k CALL))
              (let [s (new-var 's)
                    CALL `(~@call ~s)
                    RST (S-app rst k CALL)
                    K `(~'fn [~s] ~RST)]
                (S fst K))))))
  (match [expr]
    [([fst & rst] :seq)] (S-app expr k '())
    :else (throw
           (Exception. (str "Invalid serious express: " expr)))))
(defn T
  "CPS function for trivial Clojure expressions with respect to the
  Olivier-style CPS algorithm."
  [expr]
  (match [expr]
    [(s :when symbol?)] s
    [(n :when number?)] n
    [(['fn id-ls body] :seq)]
      (let [k (new-var 'k)]
        (let [BODY (E body k)]
          `(~'fn [~@id-ls ~k] ~BODY)))
    :else (throw
           (Exception. (str "Invalid trivial expression: " expr)))))

(defn cps
  "Entry-point function for the Olivier-style CPS algorithm. Returns
  an function which takes a Clojure function representing a
  continuation and evaluates the original expression with respect to
  that continuation."
  [expr]
  (do
    (reset-var-num)
    (let [k (new-var 'k)
          EXPR (E expr k)]
      `(~'fn [~k] ~EXPR))))
