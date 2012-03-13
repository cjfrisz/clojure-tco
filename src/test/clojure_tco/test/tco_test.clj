;;----------------------------------------------------------------------
;; File tco_test.clj
;; Written by Chris Frisz
;; 
;; Created 10 Mar 2012
;; Last modified 13 Mar 2012
;; 
;; Tests for the full tail-call optimization suite.
;;----------------------------------------------------------------------

(ns clojure-tco.test.tco-test
  (:use clojure.test)
  (:use clojure.set)
  (:use [clojure.core.match
         :only (match)])
  (:use clojure-tco.tco)
  (:use clojure-tco.util))

(declare free-vars)

(defn free-vars
  [expr]
  ((fn loop [expr bound* free*]
     (match [expr]
       [(s :when symbol?)] (if (some #{s} bound*) free* (union `(~s) free*))
       [(:or true false)]  free*
       [(n :when number?)] free*
       [(['fn fml* body] :seq)] (recur body (union fml* bound*) free*)
       [(['if test conseq alt] :seq)] (let [t-free* (loop test bound* free*)
                                            c-free* (loop conseq bound* free*)
                                            a-free* (loop alt bound* free*)]
                                        (union t-free* c-free* a-free*))
       [([(op :when triv-op?) & opnd*] :seq)] (apply
                                               union
                                               (map
                                                (fn [x] (loop x bound* free*))
                                                opnd*))
       [([rator & rand*] :seq)] (let [rator-free* (loop rator bound* free*)
                                      rand-free** (map
                                                   (fn [x] (loop x bound* free*))
                                                   rand*)]
                                  (apply union `(~rator-free* ~@rand-free**)))
       :else (throw
              (Exception.
               (str "Invalid expression in free-vars: " expr)))))
   expr '() '()))

(def fact-seq
  '(defn fact
     [n]
     (if (zero? n)
         1
         (* n (fact (dec n))))))

(def ackermann-seq
  '(defn ack
     [x y]
     (if (zero? x)
         (inc y)
         (if (zero? y)
             (ack (dec x) 1)
             (ack (dec x) (ack x (dec y)))))))