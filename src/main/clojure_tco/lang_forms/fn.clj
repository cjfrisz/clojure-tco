;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 31 Mar 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms.fn
  (:require [clojure-tco.lang-forms.expr :as expr])
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(defrecord Fn [fml* body]
  expr/PExpr
  (walk-expr [this f & args]
    (let [fml* (:fml* this)
          BODY (apply f (:body this) args)]
      (Fn. fml* BODY)))
  (cps [this & _]
    (let [k (new-var 'k)
          FML* (conj (:fml* this) k)
          BODY (cps body k)]
      (Fn. FML* BODY)))
  (abstract-k [this app-k] (walk-expr this app-k))
  (thunkify [this] (Fn. [] this)))
