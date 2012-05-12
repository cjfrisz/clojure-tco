;;----------------------------------------------------------------------
;; File invoke.clj
;; Written by Alan Dipert
;; 
;; Created 27 May 2012
;; 
;; Testing for general behavior correctness.
;;----------------------------------------------------------------------

(ns ctco.test.invoke
  (:use [clojure.test]
        [clojure.pprint]
        [ctco :only (ctco)]))

(ctco
 (defn countdown [i]
   (if (zero? i)
     (constantly "done")
     (countdown (dec i)))))

(ctco
 (defn fact
   [n]
   (if (zero? n)
     1
     (* n (fact (dec n))))))

(deftest function-return-value-test
  (is (= "done" ((countdown 10)))))

(deftest fact-test
  (is (= 120 (fact 5))))
