;;----------------------------------------------------------------------
;; File emit.clj
;; Written by Chris Frisz
;; 
;; Created 15 Apr 2012
;; Last modified 28 Apr 2012
;; 
;; Testing for the correctness of emit
;;----------------------------------------------------------------------

(ns ctco.test.emit
  (:use [clojure.test]
        [clojure.pprint]
        [ctco.parse :only (parse)])
  (:require [ctco.expr
             app atomic cont defn fn if simple-op]
            [ctco.protocol :as proto])
  (:import [ctco.expr.app
            App]
           [ctco.expr.atomic
            Atomic]
           [ctco.expr.cont
            Cont AppCont AppContAbs]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            Fn]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]
           [ctco.expr.thunk
            Thunk]))

(deftest app-test
  (is (= '(x y z) (proto/emit (parse '(x y z)))))
  (is (= '((clojure.core/fn [x] x) 5) (proto/emit (parse '((fn [x] x) 5)))))
  (is (= '((clojure.core/fn [x y z] (* x 3)) 7 3 12)
         (proto/emit (parse '((fn [x y z] (* x 3)) 7 3 12))))))

(deftest atomic-test
  (is (= 5 (proto/emit (parse 5))))
  (is (= 25883 (proto/emit (parse 25883))))
  (is (= '(quote stuff) (proto/emit (parse '(quote stuff))))))
 
(deftest cont-test
  (is (= '(clojure.core/fn [x] x)
         (proto/emit (Cont. (Atomic. 'x) (Atomic. 'x)))))
  (is (= '((clojure.core/fn [x] x) (quote thonk))
         (proto/emit (AppCont. (Cont. (Atomic. 'x) (Atomic. 'x))
                               (Atomic. (quote 'thonk)))))))

(deftest defn-test
  (is (= '(clojure.core/defn id [x] x)
         (proto/emit (parse '(defn id [x] x)))))
  (is (= '(clojure.core/defn id ([x] x) ([x y] x))
         (proto/emit (parse '(defn id ([x] x) ([x y] x)))))))


(deftest if-test
  (is (= '(if 3 4 5) (proto/emit (parse '(if 3 4 5)))))
  (is (= '(if ((clojure.core/fn [x] x) 3) 4 5)
         (proto/emit (parse '(if ((fn [x] x) 3) 4 5))))))

;; (deftest simple-op-test)

(deftest thunk-test
  (is (= '(clojure.core/fn [] (quote stuff))
         (proto/emit (Thunk. (Atomic. ''stuff))))))
