;;----------------------------------------------------------------------
;; File bbc.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 21 Apr 2012
;; 
;; Defines the driver for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns bbc
  (:require [bbc.expr
             [cont defn fn]]
            [bbc.mini-passes :as mp]
            [bbc.parse :as parse]
            [bbc.protocol
             [pabstract-k :as pabs-k]
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pemit :as pemit]
             [pthunkify :as pthunkify]]
            [bbc.util.new-var :as nv])
  (:import [bbc.expr.cont
            Cont AppContAbs]
           [bbc.expr.defn
            Defn]
           [bbc.expr.fn
            Fn]))

(defmacro tco
  "Entry-point for the TCO compiler. Takes a Clojure expression and
  applies the tail-call optimization algorithm to it. Thus, the
  expression given to tco will be transformed such that it is
  tail-recursive and will use a constant amount of stack memory in its
  execution."
  [expr]
  (let [tramp (nv/new-var 'tramp)
        apply-k (nv/new-var 'apply-k)
        flag (nv/new-var 'flag)]
    (letfn [(apply-cps [expr]
              (if (extends? srs/PCpsSrs (type expr))
                  (let [k (nv/new-var 'k)]
                    (srs/cps expr k))
                  (triv/cps expr)))
            (wrap-expr [expr]
              (if (instance? Defn expr)
                  expr
                  (let [k (nv/new-var 'k)
                        app (AppContAbs. apply-k k expr)]
                    (Cont. k app))))]
      (let [expr (parse/parse expr)
            expr (apply-cps expr)
            expr (pabs-k/abstract-k expr apply-k)
            expr (pthunkify/thunkify expr)
            expr (wrap-expr expr)
            expr (mp/overload expr tramp flag)
            expr (mp/make-flag expr flag)
            expr (mp/make-apply-k expr apply-k)
            expr (mp/make-trampoline expr tramp)]
        (pemit/emit expr)))))

