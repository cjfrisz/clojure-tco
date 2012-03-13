;;----------------------------------------------------------------------
;; File tco_test.clj
;; Written by Chris Frisz
;; 
;; Created 10 Mar 2012
;; Last modified 13 Mar 2012
;; 
;; Tests for the full tail-call optimization suite.
;;----------------------------------------------------------------------

(ns clojure-tco.test.tco-test
  (:use clojure.test)
  (:use clojure.set)
  (:use [clojure.core.match
         :only (match)])
  (:use clojure-tco.tco)
  (:use clojure-tco.util))

(declare
 alpha-equiv? alpha-equiv-fn? alpha-equiv-op? alpha-equiv-app?
 free-vars)

(defn alpha-equiv?
  [e1 e2]
  (or (= e1 e2)
      (match [e1 e2]
        [(s1 :when symbol?) (s :when symbol?)] true
        [(['fn ffml* fbody] :seq) (['fn sfml* sbody] :seq)]
         (alpha-equiv-fn? ffml* fbody sfml* sbody)
        [(['if t1 c1 a1] :seq) (['if t2 c2 a2] :seq)]
         (and (alpha-equiv? t1 t2) (alpha-equiv? c1 c2) (alpha-equiv? a1 a2))
        [([(op1 :when triv-op?) & opnd1*] :seq)
         ([(op2 :when triv-op?) & opnd2*] :seq)]
         (alpha-equiv-op? op1 opnd1* op2 opnd2*)
        [([rator1 & rand1*] :seq) ([rator2 & rand2*] :seq)]
         (alpha-equiv-app? rator1 rand1* rator2 rand2*)
        :else (throw
               (Exception.
                (str "Invalid expression(s) in alpha-equiv?: " e1 e2))))))

(defn alpha-equiv-fn?
  [ffml* fbody sfml* sbody]
  (if (= (count ffml*) (count sfml*))
      (let [v (new-var 'v)
            ar (fn [b f] (alpha-rename f v b))
            FBODY (reduce ar fbody ffml*)
            SBODY (reduce ar sbody sfml*)]
        (= FBODY SBODY))))

(defn alpha-equiv-op?
  [op1 opnd1* op2 opnd2*]
  (let [equiv-opnd* (map (fn [x y] (alpha-equiv? x y)) opnd1* opnd2*)]
    (and (= op1 op2) (reduce true? equiv-opnd*))))

(defn alpha-equiv-app?
  [rator1 rand1* rator2 rand2*]
  (let [equiv-rand* (map (fn [x y] (alpha-equiv? x y)) rand1* rand2*)]
    (and (alpha-equiv? rator1 rator2) (reduce true? equiv-rand*))))

(defn free-vars
  [expr]
  ((fn loop [expr bound* free*]
     (match [expr]
       [(s :when symbol?)] (if (some #{s} bound*) free* (union `(~s) free*))
       [(:or true false)]  free*
       [(n :when number?)] free*
       [(['fn fml* body] :seq)] (recur body (union fml* bound*) free*)
       [(['if test conseq alt] :seq)] (let [t-free* (loop test bound* free*)
                                            c-free* (loop conseq bound* free*)
                                            a-free* (loop alt bound* free*)]
                                        (union t-free* c-free* a-free*))
       [([(op :when triv-op?) & opnd*] :seq)] (apply
                                               union
                                               (map
                                                (fn [x] (loop x bound* free*))
                                                opnd*))
       [([rator & rand*] :seq)] (let [rator-free* (loop rator bound* free*)
                                      rand-free** (map
                                                   (fn [x] (loop x bound* free*))
                                                   rand*)]
                                  (apply union `(~rator-free* ~@rand-free**)))
       :else (throw
              (Exception.
               (str "Invalid expression in free-vars: " expr)))))
   expr '() '()))

(def fact-seq
  '(defn fact
     [n]
     (if (zero? n)
         1
         (* n (fact (dec n))))))

(def ackermann-seq
  '(defn ack
     [x y]
     (if (zero? x)
         (inc y)
         (if (zero? y)
             (ack (dec x) 1)
             (ack (dec x) (ack x (dec y)))))))