;;----------------------------------------------------------------------
;; File ctco.clj
;; Written by Chris Frisz
;; 
;; Created 28 Apr 2012
;; Last modified 9 May 2012
;; 
;; Test programs for the full CTCO compiler.
;;----------------------------------------------------------------------

(ns ctco.test.ctco
  (:use [clojure.test]
        [clojure.core.match
         :only (match)]
        [clojure.walk
         :only (prewalk-replace)]
        [ctco
         :only (ctco)]))

(defmacro ctco-test
  "Takes a valid CTCO expression and a sequence of inputs and evaluates the 
  expression with each input in the sequence both with and without the CTCO
  transformation. All StackOverflowErrors stemming from the unmodified code
  are caught and reported. Exceptions from the CTCO modified code are uncaught
  and assumed to be errors."
  [expr & input*]
  (let [ctco-expr (match [expr]
                   [(['defn name fml* body] :seq)] 
                    (prewalk-replace {name (gensym name)} expr)
                   :else expr)]
    (println input*)
    `(let [old# ~expr
           new# (ctco ~ctco-expr)]
       (loop [input*# ~input*]
         (when-not (nil? (seq input*#)) 
           (let [fail# (gensym 'hukarz)
                 input# (first input*#)
                 old-eval# (try
                             (apply old# input#)
                             (catch StackOverflowError e#
                               (println "Original overflowed on input" input#)
                               fail#))
                 new-eval# (apply new# input#)]
             (if (not (= old-eval# fail#))
               (is (= old-eval# new-eval#))
               (println "CTCO completed and produced " new-eval#))
             (recur (next input*#))))))))

