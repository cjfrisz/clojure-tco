;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified  5 Oct 2012
;; 
;; Defines the ctco macro which acts as the driver for the Clojure TCO
;; compiler. The macro parses the initial expression, and applies the
;; main Clojure TCO transformations to the expression, including the CPS
;; transformation, continuation representation abstraction, and
;; thunkification. The code is then unparsed and the final mini-passes
;; (overload, make-apply-k, and make-trampoline) are applied before the
;; transformed expression is returned.
;;----------------------------------------------------------------------

(ns ctco.core
  (:require [ctco.mini-passes :as mp]
            [ctco.parse :as parse]
            [ctco.protocol :as proto]
            [ctco.util :as util]))

(defmacro ctco
  "Entry-point for the TCO compiler. Takes a Clojure expression and
  applies the tail-call optimization algorithm to it. Thus, the
  expression given to ctco will be transformed such that it is
  tail-recursive and will use a constant amount of stack memory in its
  execution.

  Note that if the expression passed into ctco isn't initially
  tail-recursive, the resulting expression will use constant stack
  space, but recursive calls will necessitate more heap memory for the
  closures created to represent continuations."
  [expr]
  (let [tramp (gensym "tramp")]
    (letfn [(apply-cps [expr]
              (condp extends? (type expr)
                proto/PCpsTriv (proto/cps-triv expr)
                proto/PCpsSrs  (proto/cps-srs expr (gensym "k"))
                :else (throw (Exception. (str "unexpected expression " expr)))))]
      (let [new-expr (-> (parse/parse expr)
                         apply-cps
                         proto/thunkify
                         (proto/load-tramp tramp)
                         proto/unparse)]
        (let [thunk (gensym "thunk")]
          `(letfn [(~tramp [~thunk]
                     (if (:thunk (meta ~thunk))
                         (recur (~thunk))
                         ~thunk))]
             (~tramp ~new-expr)))))))
