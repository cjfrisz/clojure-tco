;;----------------------------------------------------------------------
;; File unparse.clj
;; Written by Chris Frisz
;; 
;; Created 15 Apr 2012
;; Last modified 30 Aug 2012
;; 
;; Testing for the correctness of unparse
;;----------------------------------------------------------------------

(ns ctco.test.unparse
  (:use [clojure.test]
        [clojure.pprint]
        [ctco.parse :only (parse)])
  (:require [ctco.expr
             app simple cont defn fn if simple-op]
            [ctco.protocol :as proto])
  (:import [ctco.expr.app
            App]
           [ctco.expr.simple
            Simple]
           [ctco.expr.cont
            Cont AppCont AppContAbs]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            FnBody]
           [ctco.expr.if
            IfCps IfSrs IfTriv]
           [ctco.expr.simple_op
            SimpleOpCps SimpleOpSrs SimpleOpTriv]
           [ctco.expr.thunk
            Thunk]))

(deftest app-test
  (is (= '(x y z) (proto/unparse (parse '(x y z)))))
  (is (= '((clojure.core/fn ([x] x)) 5) (proto/unparse (parse '((fn [x] x) 5)))))
  (is (= '((clojure.core/fn ([x y z] (* x 3))) 7 3 12)
         (proto/unparse (parse '((fn [x y z] (* x 3)) 7 3 12))))))

(deftest simple-test
  (is (= 5 (proto/unparse (parse 5))))
  (is (= 25883 (proto/unparse (parse 25883))))
  (is (= '(quote stuff) (proto/unparse (parse '(quote stuff))))))
 
(deftest cont-test
  (is (= '(clojure.core/fn [x] x)
         (proto/unparse (Cont. (Simple. 'x) (Simple. 'x)))))
  (is (= '((clojure.core/fn [x] x) (quote thonk))
         (proto/unparse (AppCont. (Cont. (Simple. 'x) (Simple. 'x))
                               (Simple. (quote 'thonk)))))))

(deftest defn-test
  (is (= '(clojure.core/defn id ([x] x))
         (proto/unparse (parse '(defn id [x] x)))))
  (is (= '(clojure.core/defn id ([x] x) ([x y] x))
         (proto/unparse (parse '(defn id ([x] x) ([x y] x)))))))


(deftest if-test
  (is (= '(if 3 4 5) (proto/unparse (parse '(if 3 4 5)))))
  (is (= '(if ((clojure.core/fn ([x] x)) 3) 4 5)
         (proto/unparse (parse '(if ((fn [x] x) 3) 4 5))))))

(deftest thunk-test
  (is (= '(clojure.core/with-meta (clojure.core/fn [] (quote stuff)) {:thunk true})
         (proto/unparse (Thunk. (Simple. ''stuff))))))
