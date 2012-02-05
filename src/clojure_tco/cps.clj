;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified  5 Feb 2012
;; 
;; A CPSer for Clojure in Clojure.
;;----------------------------------------------------------------------

(ns clojure-tco.cps
  (:use [clojure.core.match :only [match]])
  (:use [clojure-tco.util :only [reset-var-num new-var]]))

(declare trivial? E S T)

(defn trivial? [t]
  (match [t]
    [(b :when [true? false?])] true
    [(s :when symbol?)]        true
    [(n :when number?)]        true
    [(['fn id-ls body] :seq)]  true
    :else                      false))

(defn E [expr k]
  (if (trivial? expr)
      (let [EXPR (T expr)]
        `(~k ~EXPR))
      (S expr k)))

(defn S [expr k]
  (defn S-helper [expr k call]
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
                    RST (S-helper rst k CALL)
                    K `(~'fn [~s] ~RST)]
                (S fst K))))))
  (S-helper expr k '()))

(defn T [expr]
  (match [expr]
    [(s :when symbol?)] s
    [(n :when number?)] n
    [(['fn id-ls body] :seq)]
      (let [k (new-var 'k)]
        (let [BODY (E body k)]
          `(~'fn [~@id-ls ~k] ~BODY)))