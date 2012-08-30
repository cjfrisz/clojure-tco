;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 29 Aug 2012
;; 
;; Defines the ctco macro which acts as the driver for the Clojure TCO
;; compiler. The macro parses the initial expression, and applies the
;; main Clojure TCO transformations to the expression, including the CPS
;; transformation, continuation representation abstraction, and
;; thunkification. It also applies the set of mini-passes, including
;; overloading and making the "done" flag, continuation application
;; function, and trampoline function. Finally, ctco emits the syntax for
;; the transformed expression.
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
  (let [tramp (gensym 'tramp)
        init-k (gensym 'init-k)
        apply-k (gensym 'apply-k)]
    (letfn [(apply-cps [expr]
              (condp extends? (type expr)
                proto/PCpsTriv (proto/cps-triv expr)
                proto/PCpsSrs  (proto/cps-srs expr init-k)
                :else (throw (Exception. (str "unexpected expression " expr)))))]
      (-> (parse/parse expr)
          apply-cps
          (proto/abstract-k (parse/parse apply-k))
          proto/thunkify
          proto/emit
          (mp/overload tramp)
          (mp/make-apply-k apply-k)
          (mp/make-trampoline tramp)))))
