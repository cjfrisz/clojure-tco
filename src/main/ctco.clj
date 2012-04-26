;;----------------------------------------------------------------------
;; File ctco.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 26 Apr 2012
;; 
;; Defines the driver for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns ctco
  (:require [ctco.expr
             [cont defn fn]]
            [ctco.mini-passes :as mp]
            [ctco.parse :as parse]
            [ctco.protocol :as proto]
            [ctco.util.new-var :as nv])
  (:import [ctco.expr.cont
            Cont AppContAbs]
           [ctco.expr.defn
            Defn]
           [ctco.expr.fn
            Fn]))

(defmacro ctco
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
              (if (extends? proto/PCpsSrs (type expr))
                  (let [k (nv/new-var 'k)]
                    (proto/cps-srs expr k))
                  (proto/cps-triv expr)))
            (wrap-expr [expr]
              (if (instance? Defn expr)
                  expr
                  (let [k (nv/new-var 'k)
                        app (AppContAbs. apply-k k expr)]
                    (Cont. k app))))]
      (let [expr (parse/parse expr)
            expr (apply-cps expr)
            expr (proto/abstract-k expr apply-k)
            expr (proto/thunkify expr)
            expr (wrap-expr expr)
            expr (mp/overload expr tramp flag)
            expr (mp/make-flag expr flag)
            expr (mp/make-apply-k expr apply-k)
            expr (mp/make-trampoline expr tramp)]
        (proto/emit expr)))))

