;;----------------------------------------------------------------------
;; File cps_test.clj
;; Written by Christopher Frisz
;; 
;; Created 10 Feb 2012
;; Last modified 13 Mar 2012
;; 
;; Tests for Clojure TCO's CPSer.
;;----------------------------------------------------------------------

(ns clojure-tco.test.cps-test
  (:use clojure-tco.cps)
  (:use clojure.test)
  (:use clojure-tco.test.util))

(def empty-k (fn [x] x))

(defn cps-eval-with-k
  [expr k & args]
  (let [cpsed (cps expr)
        evaled (eval cpsed)
        EXPR (evaled k)]
    (if (and (function? evaled) (not (nil? (next args))))
        (apply EXPR args)
        EXPR)))

(defn cps-eval
  [expr & args]
  (apply cps-eval-with-k `(~expr ~empty-k ~@args)))

(deftest id-test-1
  (is (alpha-equiv? (cps '(fn [x] x)) '(fn [k!0] (k!0 (fn [x k!1] (k!1 x))))))
  "Make sure that simple tranformations come out right.")

(deftest heavy-redex
  (is (= (cps-eval '(((fn [x] x) (fn [y] y)) (((fn [u] (fn [v] u)) 5) 12)))
         5))
  "Check evaluation of a non-trivial function application")

(deftest heavy-redex-n-arity
  (is (= (cps-eval '(((fn [x] x) (fn [y] y)) ((fn [u v] u) 5 12)))
         5))
  "Check evaluation for non-trivial function application including
  functions with higher arity.")

(deftest simple-if
  (is (= (cps-eval '(if 3 4 5)) 4)))

(deftest harder-if-1
  (is (= (cps-eval '(if ((fn [x] x) 3) 4 5)))) 4)

(deftest harder-if-2
  (is (= (cps-eval '(if 3 ((fn [x] x) 4) 5)) 4)))

(deftest harder-if-3
  (is (= (cps-eval '(if 3 4 ((fn [x] x) 5))) 4)))

(deftest really-hard-if
  (is (= (cps-eval
          '(if ((fn [x y] x) true false)
               (((fn [x] x) (fn [y] y)) 5)
               (((fn [x y] (x y)) (fn [y] y) (fn [z] z)) 12)))
         5)))

(deftest simple-plus
  (is (= (cps-eval '(+ 2 3)) 5)))

(deftest lotso-plus
  (is (= (cps-eval '(+ 1 2 3 4 5 6 7 8 9)) 45)))

(deftest weird-mult
  (is (= (cps-eval '(* 2 ((fn [x] x) 3))) 6)))

(deftest weird-mult-2
  (is (= (cps-eval '(* 2 ((fn [x] x) 3) 6)) 36)))

(deftest mixed-ops
  (is (= (cps-eval '(* 2 (+ 2 3))) 10)))

(deftest mixed-ops-hard
  (is (= (cps-eval '(+ (* 2 (* 2 ((fn [x] x) 1) 3)))) 12)))

(deftest mixed-ops-stupid-hard
  (is (= (cps-eval '(+ (* 2 (* 2 ((fn [x] x) 1) 3)) ((fn [x y] x) 5 6)))
         17)))

(deftest poor-mans-y
  (is (= (cps-eval '((fn [n]
                      ((fn [fact]
                         (fact fact n))
                       (fn [fact n]
                         (if (zero? n)
                             1
                             (* n (fact fact (dec n)))))))
                     5))
         120)))
