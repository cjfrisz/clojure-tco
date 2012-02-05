;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created  3 Feb 2012
;; Last modified  4 Feb 2012
;; 
;; A CPSer for Clojure in Clojure.
;;----------------------------------------------------------------------

(ns clojure-tco.cps
  (:use [clojure.core.match :only [match]])
  (:require [clojure-tco.util :as util]))

(declare trivial? E S T)

(defn trivial? [t]
  (match [t]
    [(b :when [true? false?])] true
    [(s :when symbol?)]        true
    [(n :when number?)]        true
    [([fn id-ls body] :seq)]   true
    :else                      false))

(defn E [expr k]
  (if (trivial? expr)
      (let [EXPR (T expr)]
        `(~k ~EXPR))
      (S expr k)))

(defn S [expr k]
  (loop [expr expr
         k k
         call nil]
    (if (nil? (seq expr))
        `(~@call ~k)
        (let [[fst rst] [(first expr) (rest expr)]]
          (if (trivial? fst)
              (let [FST (T fst)]
                (let [CALL `(~@call ~FST)]
                  (recur rst k CALL)))
              (let [s (util/new-var 's)]
                (let [CALL `(~@call ~s)]
                  (let [RST (recur rst k CALL)]
                    (let [K `(fn [~s] ~RST)]
                      (S fst K))))))))))

(defn T [expr]
  (match [expr]
    [(x :when [number? symbol?])] x
    [([fn id-ls body] :seq)]
      (let [k (util/new-var 'k)]
        (let [BODY (E body k)]
          `(fn [~@id-ls ~k] ~BODY)))))