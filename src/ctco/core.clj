;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 21 Aug 2012
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
  (:require #_[ctco.expr [cont defn]]
            [ctco.mini-passes :as mp]
            [ctco.parse :as parse]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.cont
            Cont AppContAbs]
           [ctco.expr.defn
            Defn]))

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
  (let [tramp (util/new-var 'tramp)
        init-k (util/new-var 'init-k)
        apply-k (util/new-var 'apply-k)]
    (letfn [(apply-cps [expr]
              (if (extends? proto/PCpsSrs (type expr))
                  (proto/cps-srs expr init-k)
                  (proto/cps-triv expr)))
            (wrap-expr [expr]
              (if (instance? Defn expr)
                  expr
                  (let [k (util/new-var 'k)]
                    (Cont. k (AppContAbs. apply-k k expr)))))]
      (-> (parse/parse expr)
          apply-cps
          (proto/abstract-k apply-k)
          proto/thunkify
          wrap-expr
          (mp/overload tramp)
          (mp/make-apply-k apply-k)
          (mp/make-trampoline tramp)
          proto/emit))))
