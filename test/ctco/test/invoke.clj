;;----------------------------------------------------------------------
;; File invoke.clj
;; Written by Alan Dipert
;;
;; Created 27 May 2012
;; Last modified 28 Aug 2012
;;
;; Testing for general behavior correctness.
;;----------------------------------------------------------------------

(ns ctco.test.invoke
  (:use [clojure.test]
        [clojure.pprint]
        [ctco.core :only (ctco)]))

(ctco
 (defn countdown [i]
   (if (zero? i) :done (countdown (dec i)))))

(def done (constantly :done))

(ctco
 (defn countdown-fn [i]
   (if (zero? i) done (countdown-fn (dec i)))))

(ctco
 (defn fact
   [n]
   (if (zero? n)
     1
     (* n (fact (dec n))))))

(deftest value-returning
  (testing "functions with simple expressions"
    (is (= 120 (fact 5)))
    (is (= :done (countdown 10)))))

(deftest function-returning
  (testing "functions with simple expressions that return functions"
    (is (= :done ((countdown-fn 10))))))
