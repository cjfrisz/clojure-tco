;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 13 Mar 2012
;; Last modified 13 Mar 2012
;; 
;; Miscellaneous utilities for testing Clojure TCO.
;;----------------------------------------------------------------------

(ns clojure-tco.test.util
  (:use clojure.set)
  (:use [clojure.core.match
         :only (match)])
  (:use clojure-tco.util))

(declare
 alpha-equiv? alpha-equiv-fn? alpha-equiv-op? alpha-equiv-app?)

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
        [(['defn fname ffml* fbody] :seq) (['defn sname sfml* sbody] :seq)]
         (alpha-equiv-fn? ffml* fbody sfml* sbody) 
        :else false)))

(defn- alpha-equiv-fn?
  [ffml* fbody sfml* sbody]
  (and (= (count ffml*) (count sfml*))
       (let [new-fml* (for [x ffml*] (new-var 'v))]
         (letfn [(alpha [fml* body]
                   (loop [fml* fml*
                          new-fml* new-fml*
                          body body]
                     (if (nil? (seq fml*))
                         body
                         (let [old (first fml*)
                               new (first new-fml*)
                               BODY (alpha-rename old new body)]
                           (recur (rest fml*) (rest new-fml*) BODY)))))]
           (let [FBODY (alpha ffml* fbody)
                 SBODY (alpha sfml* sbody)]
             (alpha-equiv? FBODY SBODY))))))

(defn- alpha-equiv-op?
  [op1 opnd1* op2 opnd2*]
  (let [equiv-opnd* (map (fn [x y] (alpha-equiv? x y)) opnd1* opnd2*)]
    (and (= op1 op2) (every? true? equiv-opnd*))))

(defn- alpha-equiv-app?
  [rator1 rand1* rator2 rand2*]
  (let [equiv-rand* (map (fn [x y] (alpha-equiv? x y)) rand1* rand2*)]
    (and (alpha-equiv? rator1 rator2) (every? true? equiv-rand*))))
