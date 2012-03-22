;;----------------------------------------------------------------------
;; File tramp.clj
;; Written by Chris Frisz
;; 
;; Created  6 Feb 2012
;; Last modified 22 Mar 2012
;; 
;; Defines utilities for trampolining Clojure code. Primarily, this
;; refers to "tramp," which takes a sequence representing a Clojure
;; expression (assumed to be CPSed and thunkified and the symbolic
;; name of the trampoline function and returns the expression rigged
;; to execute one step at a time via the named trampoline function.
;;----------------------------------------------------------------------

(ns clojure-tco.tramp
  (:use [clojure.core.match
         :only (match)])
  (:use [clojure-tco.util
         :only (new-var triv-op? alpha-rename)]))

(declare tramp tramp-fn tramp-if tramp-op tramp-defn tramp-app)

;;-------------------------------------------------------
;; TRAMP: Sets up code for trampolining (and helpers)
;;-------------------------------------------------------
(defn tramp
  "Takes a sequence representing a Clojure expression (assumed to be
  CPSed and thunkified) and returns the trampolined version of the
  expression. That is, it returns the expression such that it executes
  one step at a time."
  [expr bounce]
  (match [expr]
    [(:or true false)] expr
    [(n :when number?)] n
    [(s :when symbol?)] s
    [(['fn fml* body] :seq)] (tramp-fn fml* body bounce)
    [(['if test conseq alt] :seq)] (tramp-if test conseq alt bounce)
    [([(op :when triv-op?) & opnd*] :seq)] (tramp-op op opnd* bounce)
    [(['defn name fml* body] :seq)] (tramp-defn name fml* body bounce)
    [([rator & rand*] :seq)] (tramp-app rator rand* bounce)
    :else (throw (Exception. (str "Invalid expression in tramp: " expr)))))

(defn- tramp-fn
  "Helper function for tramp that handles functions."
  [fml* body bounce]
  (if (> (count fml*) 0)
      (let [done (new-var 'done)
            fnv (new-var 'fnv)
            thunk (new-var 'th)
            BODY (tramp body bounce)] 
        `(~'fn ~fml*
           (~'def ~done (~'ref false))
           (~'let [~fnv (~'fn ~fml* ~BODY)]
             (~'let [~thunk (~fnv ~fml*)]
               (~bounce ~thunk ~done)))))
      (let [BODY (tramp body bounce)]
        `(~'fn ~fml* ~BODY))))

(defn- tramp-if
  "Helper function for tramp that handles 'if' expressions"
  [test conseq alt bounce]
  (let [TEST (tramp test bounce)
        CONSEQ (tramp conseq bounce)
        ALT (tramp alt bounce)]
    `(~'if ~TEST ~CONSEQ ~ALT)))

(defn- tramp-op
  "Helper function for tramp that handles simple operations (i.e.
  arithmetic +, -, *, etc.)"
  [op opnd* bounce]
  (let [OPND* (map (fn [opnd] (tramp opnd bounce)) opnd*)]
    `(~op ~@OPND*)))

(defn- tramp-defn
  "Helper function for tramp that handles 'defn' expressions."
  [name fml* body bounce]
  (let [done (new-var 'done)
        fnv (new-var name)
        thunk (new-var 'th)
        body-rn (alpha-rename name fnv body)
        BODY-RN (tramp body-rn bounce)]
    `(~'defn ~name
       ~fml*
       (~'def ~done (~'ref ~'false))
       (~'letfn [(~fnv ~fml* ~BODY-RN)]
         (~'let [~thunk  (~fnv ~fml*)]
           (~bounce ~thunk ~done))))))

(defn- tramp-app
  "Helper function for tramp that handles function application."
  [rator rand* bounce]
  (let [RATOR (tramp rator bounce)
        kont (last rand*)
        rand-bl* (butlast rand*)
        RAND-BL* (map
                  (fn [n] (tramp n bounce))
                  rand-bl*)]
    `(~RATOR ~@RAND-BL* ~kont)))


