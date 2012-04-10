;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 10 Apr 2012
;; 
;; Testing for the CPSer in the record+protocol'd version of the TCO
;; compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.test.cps
  (:use clojure.test)
  (:require [clojure-tco.expr
             app atomic cont defn fn if simple-op]
            [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]])
  (:import [clojure_tco.expr.app
            App]
           [clojure_tco.expr.atomic
            Bool Num Sym Var]
           [clojure_tco.expr.cont
            Cont AppCont]
           [clojure_tco.expr.defn
            Defn]
           [clojure_tco.expr.fn
            Fn]
           [clojure_tco.expr.if
            IfCps IfSrs IfTriv]
           [clojure_tco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(let [test-bool (Bool. true)]
  (deftest atomic-bool
    (is (true? (extends? triv/PCpsTriv (type test-bool))))
    (is (false? (extends? srs/PCpsSrs (type test-bool))))
    (is (= (triv/cps test-bool) test-bool))))

(let [test-num (Num. true)]
  (deftest atomic-num
    (is (true? (extends? triv/PCpsTriv (type test-num))))
    (is (false? (extends? srs/PCpsSrs (type test-num))))
    (is (= (triv/cps test-num) test-num))))

(let [test-sym (Sym. true)]
  (deftest atomic-sym
    (is (true? (extends? triv/PCpsTriv (type test-sym))))
    (is (false? (extends? srs/PCpsSrs (type test-sym))))
    (is (= (triv/cps test-sym) test-sym))))

(let [test-var (Var. true)]
  (deftest atomic-var
    (is (true? (extends? triv/PCpsTriv (type test-var))))
    (is (false? (extends? srs/PCpsSrs (type test-var))))
    (is (= (triv/cps test-var) test-var))))

(let [test-fn-triv (Fn. [(Var. 'x)] (Var. 'x))]
  (deftest fn-triv
    (is (true? (extends? triv/PCpsTriv (type test-fn-triv))))
    (is (false? (extends? srs/PCpsSrs (type test-fn-triv))))
    (is (= (count (:fml* test-fn-triv)) 1))
    (let [test-fn-cps (triv/cps test-fn-triv)]
      (is (not (= test-fn-triv test-fn-cps)))
      (is (= (count (:fml* test-fn-cps)) 2))
      (is (= (type (:body test-fn-cps)) clojure_tco.expr.cont.AppCont)))))

(let [test-if-triv (IfTriv. (Num. 3) (Num. 4) (Num. 5))]
  (deftest if-triv
    (is (true? (extends? triv/PCpsTriv (type test-if-triv))))
    (is (false? (extends? srs/PCpsSrs (type test-if-triv))))
    (let [test-if-cps (triv/cps test-if-triv)]
      (is (= (:test test-if-triv) (:test test-if-cps)))
      (is (= (:conseq test-if-triv) (:conseq test-if-cps)))
      (is (= (:alt test-if-triv) (:alt test-if-cps)))
      (is (= (type test-if-cps) clojure_tco.expr.if.IfCps)))))

(let [test-defn (Defn. 'id (Fn. [(Var. 'x)] (Var. 'x)))]
  (deftest defn-triv
    (is (true? (extends? triv/PCpsTriv (type test-defn))))
    (is (false? (extends? srs/PCpsSrs (type test-defn))))
    (is (= (triv/cps (:func test-defn)) (:func (triv/cps test-defn))))))
