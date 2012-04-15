(ns clojure-tco.test.emit
  (:use [clojure.test]
        [clojure.pprint]
        [clojure-tco.parse :only (parse)])
  (:require [clojure-tco.expr
             app atomic cont defn fn if simple-op]
            [clojure-tco.protocol
             [pemit :as pemit]]
            [clojure-tco.util
             [new-var :as nv]])
  (:import [clojure_tco.expr.app
            App]
           [clojure_tco.expr.atomic
            Bool Num Sym Var]
           [clojure_tco.expr.cont
            Cont AppCont AppContAbs]
           [clojure_tco.expr.defn
            Defn]
           [clojure_tco.expr.fn
            Fn]
           [clojure_tco.expr.if
            IfCps IfSrs IfTriv]
           [clojure_tco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]
           [clojure_tco.expr.thunk
            Thunk]))

(deftest app-test
  (is (= '(x y z) (pemit/emit (parse '(x y z)))))
  (is (= '((clojure.core/fn [x] x) 5) (pemit/emit (parse '((fn [x] x) 5)))))
  (is (= '((clojure.core/fn [x y z] (* x 3)) 7 3 12) (pemit/emit (parse '((fn [x y z] (* x 3)) 7 3 12))))))

(deftest atomic-test
  (is (= 5 (pemit/emit (parse 5))))
  (is (= 25883 (pemit/emit (parse 25883))))
  (is (= 'stuff (pemit/emit (parse (quote 'stuff))))))
 
(deftest cont-test
  (is (= '(clojure.core/fn [x] x) (pemit/emit (Cont. (Var. 'x) (Var. 'x)))))
  (is (= '((clojure.core/fn [x] x) (quote thonk)) (pemit/emit (AppCont. (Cont. (Var. 'x) (Var. 'x)) (Sym. (quote 'thonk)))))))

(deftest defn-test
  (is (= '(clojure.core/defn id [x] x) (pemit/emit (parse '(defn id [x] x)))))
  (is (= '(clojure.core/defn id ([x] x) ([x y] x)) (pemit/emit (parse '(defn id ([x] x) ([x y] x))))))
  (print (parse '(defn id [x] x)))
  (print (parse '(defn id ([x] x) ([x y] x)))))

(deftest fn-test
  (is (= '(clojure.core/fn [x y] y) (pemit/emit (parse '(fn [x y] y))))))

(deftest if-test
  (is (= '(if 3 4 5) (pemit/emit (parse '(if 3 4 5)))))
  (is (= '(if ((clojure.core/fn [x] x) 3) 4 5) (pemit/emit (parse '(if ((fn [x] x) 3) 4 5))))))

;; (deftest simple-op-test)

(deftest thunk-test
  (is (= '(clojure.core/fn [] (quote stuff)) (pemit/emit (Thunk. (Sym. ''stuff))))))
