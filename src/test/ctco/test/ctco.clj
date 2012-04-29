;;----------------------------------------------------------------------
;; File ctco.clj
;; Written by Chris Frisz
;; 
;; Created 28 Apr 2012
;; Last modified 28 Apr 2012
;; 
;; Test programs for the full CTCO compiler.
;;----------------------------------------------------------------------

(ns ctco.test.ctco
  (:use [clojure.test]
        [ctco :only (ctco)]))

(let [fact-seq '(defn fact
                  [n]
                  (if (zero? n)
                      1
                      (* n (fact (dec n)))))
      inputs (range 10000)
      fact (eval fact-seq)
      fact-out (for [n inputs]
                 (try
                   (do
                     (println (str "Running fact of " n))
                     (fact (bigint n)))
                   (catch Exception e
                     (do
                       (println "Stack overflow!")
                       0))))
      fact (eval `(ctco ~fact-seq))
      fact-ctco-out (for [n inputs]
                      (do
                        (println (str "Running fact CTCO of " n))
                        (fact (bigint n))))]
  (deftest fact-test
    (loop [fact-out fact-out
           fact-ctco-out fact-ctco-out]
      (when-not (and (nil? (seq fact-out)) (nil? (seq fact-ctco-out)))
        (do
          (is (or (= (first fact-out) (first fact-ctco-out))
                  (zero? (first fact-ctco-out))))
          (recur (next fact-out) (next fact-ctco-out)))))))

(let [ack-seq '(defn ack
                 [m n]
                 (cond
                  (zero? m) (inc n)
                  (and (> m 0) (zero? n)) (ack (dec m) 1)
                  (and (> m 0) (> n 0) (ack (dec m) (ack m (dec n))))))])

(let [myeven?-seq '(defn myeven?
                           [n]
                           (if (zero? n)
                               true
                               (myodd? (dec n))))
      myodd?-seq '(defn myodd?
                    [n]
                    (if (zero? n)
                        false
                        (myeven? (dec n))))])
