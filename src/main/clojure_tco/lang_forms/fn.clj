;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 30 Mar 2012
;; 
;; Defines the Fn record for the Clojure TCO compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.lang-forms.fn
  (:require [clojure-tco.lang-forms.expr :as expr])
  (:require [clojure-tco.util :as util
             :only (new-var)]))

(defrecord Fn [fml* body]
  expr/Expr
  (cps [this]
    (let [k (new-var 'k)
          FML* (conj (:fml* this) k)
          BODY (cps body)]
      (Fn. FML* BODY)))
  (abstract-k [this app-k]
    (let [fml* (:fml* this)
          BODY (abstract-k (:body this) app-k)]
      (Fn. fml* BODY)))
  (thunkify [this] (Fn. [] this)))