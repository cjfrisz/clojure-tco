(ns bbc.test.emit
  (:use [clojure.test]
        [clojure.pprint]
        [bbc.parse :only (parse)])
  (:require [bbc.expr
             app atomic cont defn fn if simple-op]
            [bbc.protocol
             [pemit :as pemit]]
            [bbc.util
             [new-var :as nv]])
  (:import [bbc.expr.app
            App]
           [bbc.expr.atomic
            Atomic]
           [bbc.expr.cont
            Cont AppCont AppContAbs]
           [bbc.expr.defn
            Defn]
           [bbc.expr.fn
            Fn]
           [bbc.expr.if
            IfCps IfSrs IfTriv]
           [bbc.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]
           [bbc.expr.thunk
            Thunk]))

(deftest app-test
  (is (= '(x y z) (pemit/emit (parse '(x y z)))))
  (is (= '((clojure.core/fn [x] x) 5) (pemit/emit (parse '((fn [x] x) 5)))))
  (is (= '((clojure.core/fn [x y z] (* x 3)) 7 3 12) (pemit/emit (parse '((fn [x y z] (* x 3)) 7 3 12))))))

(deftest atomic-test
  (is (= 5 (pemit/emit (parse 5))))
  (is (= 25883 (pemit/emit (parse 25883))))
  (is (= 'stuff (pemit/emit (parse '(quote stuff))))))
 
(deftest cont-test
  (is (= '(clojure.core/fn [x] x) (pemit/emit (Cont. (Atomic. 'x) (Atomic. 'x)))))
  (is (= '((clojure.core/fn [x] x) (quote thonk)) (pemit/emit (AppCont. (Cont. (Atomic. 'x) (Atomic. 'x)) (Atomic. (quote 'thonk)))))))

(deftest defn-test
  (is (= '(clojure.core/defn id [x] x) (pemit/emit (parse '(defn id [x] x)))))
  (is (= '(clojure.core/defn id ([x] x) ([x y] x)) (pemit/emit (parse '(defn id ([x] x) ([x y] x)))))))


(deftest if-test
  (is (= '(if 3 4 5) (pemit/emit (parse '(if 3 4 5)))))
  (is (= '(if ((clojure.core/fn [x] x) 3) 4 5) (pemit/emit (parse '(if ((fn [x] x) 3) 4 5))))))

;; (deftest simple-op-test)

(deftest thunk-test
  (is (= '(clojure.core/fn [] (quote stuff)) (pemit/emit (Thunk. (Atomic. ''stuff))))))
