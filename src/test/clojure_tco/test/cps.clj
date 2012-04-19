;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 18 Apr 2012
;; 
;; Testing for the CPSer in the record+protocol'd version of the TCO
;; compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.test.cps
  (:use [clojure.test]
        [clojure.pprint])
  (:require [clojure-tco.expr
             app atomic cont defn fn if simple-op]
            [clojure-tco.protocol
             [pcps-srs :as srs]
             [pcps-triv :as triv]]
            [clojure-tco.util
             [new-var :as nv]])
  (:import [clojure_tco.expr.app
            App]
           [clojure_tco.expr.atomic
            Atomic]
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

(let [test-bool (Atomic. true)]
  (deftest atomic-bool
    (is (extends? triv/PCpsTriv (type test-bool)))
    (is (not (extends? srs/PCpsSrs (type test-bool))))
    (is (= (triv/cps test-bool) test-bool))))

(let [test-num (Atomic. 5)]
  (deftest atomic-num
    (is (extends? triv/PCpsTriv (type test-num)))
    (is (not (extends? srs/PCpsSrs (type test-num))))
    (is (= (triv/cps test-num) test-num))))

(let [test-sym (Atomic. '(quote s))]
  (deftest atomic-sym
    (is (extends? triv/PCpsTriv (type test-sym)))
    (is (not (extends? srs/PCpsSrs (type test-sym))))
    (is (= (triv/cps test-sym) test-sym))))

(let [test-var (Atomic. 'x)]
  (deftest atomic-var
    (is (extends? triv/PCpsTriv (type test-var)))
    (is (not (extends? srs/PCpsSrs (type test-var))))
    (is (= (triv/cps test-var) test-var))))

(let [test-fn-triv (Fn. [(Atomic. 'x)] (Atomic. 'x))]
  (deftest fn-triv
    (is (extends? triv/PCpsTriv (type test-fn-triv)))
    (is (not (extends? srs/PCpsSrs (type test-fn-triv))))
    (is (= (count (:fml* test-fn-triv)) 1))
    (let [test-fn-cps (triv/cps test-fn-triv)]
      (is (not (= test-fn-triv test-fn-cps)))
      (is (= (count (:fml* test-fn-cps)) 2))
      (is (instance? AppCont (:body test-fn-cps))))))

(let [test-if-triv (IfTriv. (Atomic. 3) (Atomic. 4) (Atomic. 5))]
  (deftest if-triv
    (is (extends? triv/PCpsTriv (type test-if-triv)))
    (is (not (extends? srs/PCpsSrs (type test-if-triv))))
    (let [test-if-cps (triv/cps test-if-triv)]
      (is (= (:test test-if-triv) (:test test-if-cps)))
      (is (= (:conseq test-if-triv) (:conseq test-if-cps)))
      (is (= (:alt test-if-triv) (:alt test-if-cps)))
      (is (instance? IfCps test-if-cps)))))

(let [test-defn (Defn. 'id [(Fn. [(Atomic. 'x)] (Atomic. 'x))])]
  (deftest defn-triv
    (is (extends? triv/PCpsTriv (type test-defn)))
    (is (not (extends? srs/PCpsSrs (type test-defn))))
    (let [test-defn-cps (triv/cps test-defn)]
      (is (instance? AppCont (:body (first (:func* test-defn-cps))))))))

(let [test-op (SimpleOpTriv. '+ [(Atomic. 3) (Atomic. 4) (Atomic. 5)])]
  (deftest simple-op-triv
    (is (extends? triv/PCpsTriv (type test-op)))
    (is (not (extends? srs/PCpsSrs (type test-op))))
    (let [test-op-cps (triv/cps test-op)]
      (for [opnd (:opnd* test-op)
            opnd-cps (:opnd* test-op-cps)]
        (is (= opnd opnd-cps))))))

(let [test-app (App. (Fn. [(Atomic. 'x)] (Atomic. 'x)) [(Atomic. 5)])]
  (deftest app
    (is (extends? srs/PCpsSrs (type test-app)))
    (let [k (nv/new-var 'k)
          app-cps (srs/cps test-app k)]
      (is (instance? App app-cps)))))

(let [test-if-srs (IfSrs. (SimpleOpTriv. 'zero? [(Atomic. 'x)])
                          (App. (Fn. [(Atomic. 'x)] (Atomic. 'x)) [(Atomic. 5)])
                          (Atomic. 12))]
  (deftest if-srs
    (is (extends? srs/PCpsSrs (type test-if-srs)))
    (is (extends? triv/PCpsTriv (type (:test test-if-srs))))
    (is (extends? srs/PCpsSrs (type (:conseq test-if-srs))))
    (is (extends? triv/PCpsTriv (type (:alt test-if-srs))))
    (let [k (nv/new-var 'k)
          test-if-cps (srs/cps test-if-srs k)]
      #_(pprint test-if-cps))))

(let [test-if-srs (IfSrs. (App. (Fn. [(Atomic. 'x)]
                                     (SimpleOpTriv. 'zero? [(Atomic. 'x)]))
                                [(Atomic. 35)])
                          (App. (Fn. [(Atomic. 'x)] (Atomic. 'x)) [(Atomic. 5)])
                          (Atomic. 12))]
  (deftest if-srs2
    (is (extends? srs/PCpsSrs (type test-if-srs)))
    (is (extends? srs/PCpsSrs (type (:test test-if-srs))))
    (is (extends? srs/PCpsSrs (type (:conseq test-if-srs))))
    (is (extends? triv/PCpsTriv (type (:alt test-if-srs))))
    (let [k (nv/new-var 'k)
          test-if-cps (srs/cps test-if-srs k)]
      #_(pprint test-if-cps))))

(let [test-op-srs (SimpleOpSrs. '+ [(Atomic. 3)
                                    (App. (Fn. [(Atomic. 'x)] (Atomic. 'x)) [(Atomic. 5)])
                                    (Atomic. 5)])]
  (deftest simple-op-srs
    (is (extends? srs/PCpsSrs (type test-op-srs)))
    (is (extends? triv/PCpsTriv (type (nth (:opnd* test-op-srs) 0))))
    (is (extends? srs/PCpsSrs (type (nth (:opnd* test-op-srs) 1))))
    (is (extends? triv/PCpsTriv (type (nth (:opnd* test-op-srs) 2))))
    (let [k (nv/new-var 'k)
          test-op-cps (srs/cps test-op-srs k)]
      #_(pprint test-op-cps))))
