;;----------------------------------------------------------------------
;; File test-runner.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified 10 Feb 2012
;; 
;; Defines macros for testing the correctness of clojure-tco
;; tranformations and checking the executation time and memory usage
;; for transformed programs versus umodified ones.
;;----------------------------------------------------------------------

(ns clojure-tco.test.test-runner
  (:use [clojure.pprint])
  (:use [clojure-tco.cps])
  (:use [clojure-tco.test.test-suite]))

(defmacro test-cps
  ([expr]
     (let [original expr
           cpsed (cps expr)]
       (let [original-eval (eval expr)
             cpsed-eval (eval (cps expr))]
         (do
           (print "Original: ") (pprint original)
           (print "CPS: ") (pprint cpsed)
           (println "Result:")
           (if (= original-eval cpsed-eval)
               (do
                 (println "  Passed!")
                 (println "  Value: " original-eval))
               (do
                 (println "  Failed!")
                 (print "Original: ") (println original-eval)
                 (print "CPSed: ") (println cpsed-eval))))))))