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

;; Global variables for aggregate execution time of tests
#_(def *ctco-total-exec-time* 0)
#_(def *non-ctco-total-exec-time* 0)

;; Global variables for logging
#_(def log-output #{:aggregate :individual :full})
#_(def log-level (zipmap log-output (iterate inc 1)))
#_(def *ctco-test-log-level* (log-level :aggregate))

(defmacro ctco-test
  [expr input*]
  (letfn [(time-expr [e]
            `(let [start# (. System (nanoTime))
                   e# ~e
                   end# (. System (nanoTime))]
               (list e# (- end# start#))))]
    `(letfn [(apply-expr# [e# input*# out#]
               (if (nil? (seq input*#))
                   (reverse out#)
                   (let [start# (. System (nanoTime))
                         val# (apply e# (first input*#))
                         end# (. System (nanoTime))
                         OUT# (cons (list val# (- end# start#)) out#)]
                    (recur e# (rest input*#) OUT#))))]
      (let [old# ~(time-expr expr)
            val-time-old*# (apply-expr# old# ~input* '()) 
            new# ~(time-expr (macroexpand `(ctco ~expr)))
            val-time-new*# (apply-expr# new# ~input* '())]
        (if (and (not (fn? old#)) (not (fn? new#)))
            (is (= old# new#))
            (loop [val-time-old*# val-time-old*#
                   val-time-new*# val-time-new*#]
              (do
                (is (= (ffirst val-time-old*#) (ffirst val-time-new*#)))
                (recur (next val-time-old*#) (next val-time-new*#)))))))))
               
#_(defmacro ctco-test
  "Takes a valid CTCO expression and a sequence of inputs and evaluates the 
  expression with each input in the sequence both with and without the CTCO
  transformation. All StackOverflowErrors stemming from the unmodified code
  are caught and reported. Exceptions from the CTCO modified code are uncaught
  and assumed to be errors."
  [expr input*]
  ;; Quick and dirty alpha-renaming for 'defn' expressions to avoid name 
  ;; collisions in recursive calls.
  (let [ctco-expr (match [expr]
                   [(['defn name fml* body] :seq)] 
                    (prewalk-replace {name (gensym name)} expr)
                   :else expr)]
    `(let [old# ~expr
           new# (ctco ~ctco-expr)]
       (if (and (fn? old#) (fn? new#))
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
               (recur (next input*#)))))
         (is (= old# new#))))))
