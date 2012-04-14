;;----------------------------------------------------------------------
;; File driver.clj
;; Written by Chris Frisz
;; 
;; Created 11 Apr 2012
;; Last modified 13 Apr 2012
;; 
;; Defines the driver for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.driver
  (:require [clojure-tco.expr
             [cont defn fn]]
            [clojure-tco.parse :as parse]
            [clojure-tco.protocol
             [pabstract-k :as pabs-k]
             [pcps-srs :as srs]
             [pcps-triv :as triv]
             [pemit :as pemit]
             [pthunkify :as pthunkify]]
            [clojure-tco.util.new-var :as nv])
  (:import [clojure_tco.expr.cont
            Cont AppContAbs]
           [clojure_tco.expr.defn
            Defn]
           [clojure_tco.expr.fn
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
        done (nv/new-var 'done)
        k (nv/new-var 'k)]
    (letfn [(apply-cps [expr]
              (cond
                (instance? Defn expr) (let [func (:func expr)
                                            FML* (conj (:fml* func) k)
                                            BODY (apply-cps (:body func))
                                            FUNC (Fn. FML* BODY)]
                                        (Defn. (:name expr) FUNC))
                (extends? srs/PCpsSrs (type expr)) (srs/cps expr k)
                :else (let [EXPR (triv/cps expr)
                            app (AppContAbs. apply-k k EXPR)]
                        (Cont. k app))))]
      (let [expr (parse/parse expr)
            expr (apply-cps expr)
            ;;expr (pabs-k/abstract-k expr apply-k)
            ]
        expr))))
