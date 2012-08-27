;;----------------------------------------------------------------------
;; File cps.clj
;; Written by Chris Frisz
;; 
;; Created 10 Apr 2012
;; Last modified 26 Aug 2012
;; 
;; Testing for the CPSer in the record+protocol'd version of the TCO
;; compiler.
;;----------------------------------------------------------------------

(ns ctco.test.cps
  (:use [clojure.test]
        [clojure.pprint])
  (:require [ctco.expr
             app simple cont defn fn if simple-op]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.app
            App]
           [ctco.expr.simple
            Simple]
           [ctco.expr.cont
            Cont AppCont]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            FnBody]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]))

(let [test-bool (Simple. true)]
  (deftest simple-bool
    (is (extends? proto/PCpsTriv (type test-bool)))
    (is (not (extends? proto/PCpsSrs (type test-bool))))
    (is (= (proto/cps-triv test-bool) test-bool))))

(let [test-num (Simple. 5)]
  (deftest simple-num
    (is (extends? proto/PCpsTriv (type test-num)))
    (is (not (extends? proto/PCpsSrs (type test-num))))
    (is (= (proto/cps-triv test-num) test-num))))

(let [test-sym (Simple. '(quote s))]
  (deftest simple-sym
    (is (extends? proto/PCpsTriv (type test-sym)))
    (is (not (extends? proto/PCpsSrs (type test-sym))))
    (is (= (proto/cps-triv test-sym) test-sym))))

(let [test-var (Simple. 'x)]
  (deftest simple-var
    (is (extends? proto/PCpsTriv (type test-var)))
    (is (not (extends? proto/PCpsSrs (type test-var))))
    (is (= (proto/cps-triv test-var) test-var))))

(let [test-fn-triv (FnBody. [(Simple. 'x)] (Simple. 'x))]
  (deftest fn-triv
    (is (extends? proto/PCpsTriv (type test-fn-triv)))
    (is (not (extends? proto/PCpsSrs (type test-fn-triv))))
    (is (= (count (:fml* test-fn-triv)) 1))
    (let [test-fn-cps (proto/cps-triv test-fn-triv)]
      (is (not (= test-fn-triv test-fn-cps)))
      (is (= (count (:fml* test-fn-cps)) 2))
      (is (instance? AppCont (:body test-fn-cps))))))

(let [test-if-triv (IfTriv. (Simple. 3) (Simple. 4) (Simple. 5))]
  (deftest if-triv
    (is (extends? proto/PCpsTriv (type test-if-triv)))
    (is (not (extends? proto/PCpsSrs (type test-if-triv))))
    (let [test-if-cps (proto/cps-triv test-if-triv)]
      (is (= (:test test-if-triv) (:test test-if-cps)))
      (is (= (:conseq test-if-triv) (:conseq test-if-cps)))
      (is (= (:alt test-if-triv) (:alt test-if-cps)))
      (is (instance? IfCps test-if-cps)))))

(let [test-defn (Defn. 'id [(FnBody. [(Simple. 'x)] (Simple. 'x))])]
  (deftest defn-triv
    (is (extends? proto/PCpsTriv (type test-defn)))
    (is (not (extends? proto/PCpsSrs (type test-defn))))
    (let [test-defn-cps (proto/cps-triv test-defn)]
      (is (instance? AppCont (:body (first (:func* test-defn-cps))))))))

(let [test-op (SimpleOpTriv. '+ [(Simple. 3) (Simple. 4) (Simple. 5)])]
  (deftest simple-op-triv
    (is (extends? proto/PCpsTriv (type test-op)))
    (is (not (extends? proto/PCpsSrs (type test-op))))
    (let [test-op-cps (proto/cps-triv test-op)]
      (for [opnd (:opnd* test-op)
            opnd-cps (:opnd* test-op-cps)]
        (is (= opnd opnd-cps))))))

(let [test-app (App. (FnBody. [(Simple. 'x)] (Simple. 'x)) [(Simple. 5)])]
  (deftest app
    (is (extends? proto/PCpsSrs (type test-app)))
    (let [k (util/new-var 'k)
          app-cps (proto/cps-srs test-app k)]
      (is (instance? App app-cps)))))

(let [test-if-srs (IfSrs. (SimpleOpTriv. 'zero? [(Simple. 'x)])
                          (App. (FnBody. [(Simple. 'x)] (Simple. 'x)) [(Simple. 5)])
                          (Simple. 12))]
  (deftest if-srs
    (is (extends? proto/PCpsSrs (type test-if-srs)))
    (is (extends? proto/PCpsTriv (type (:test test-if-srs))))
    (is (extends? proto/PCpsSrs (type (:conseq test-if-srs))))
    (is (extends? proto/PCpsTriv (type (:alt test-if-srs))))))

(let [test-if-srs (IfSrs. (App. (FnBody. [(Simple. 'x)]
                                     (SimpleOpTriv. 'zero? [(Simple. 'x)]))
                                [(Simple. 35)])
                          (App. (FnBody. [(Simple. 'x)] (Simple. 'x)) [(Simple. 5)])
                          (Simple. 12))]
  (deftest if-srs2
    (is (extends? proto/PCpsSrs (type test-if-srs)))
    (is (extends? proto/PCpsSrs (type (:test test-if-srs))))
    (is (extends? proto/PCpsSrs (type (:conseq test-if-srs))))
    (is (extends? proto/PCpsTriv (type (:alt test-if-srs))))))

(let [test-op-srs (SimpleOpSrs. '+ [(Simple. 3)
                                    (App. (FnBody. [(Simple. 'x)] (Simple. 'x))
                                          [(Simple. 5)])
                                    (Simple. 5)])]
  (deftest simple-op-srs
    (is (extends? proto/PCpsSrs (type test-op-srs)))
    (is (extends? proto/PCpsTriv (type (nth (:opnd* test-op-srs) 0))))
    (is (extends? proto/PCpsSrs (type (nth (:opnd* test-op-srs) 1))))
    (is (extends? proto/PCpsTriv (type (nth (:opnd* test-op-srs) 2))))))
