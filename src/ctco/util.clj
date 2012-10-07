;;----------------------------------------------------------------------
;; File util.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified  6 Oct 2012
;; 
;; Defines miscellaneous utility functions for use in CTCO. These
;; include:
;;      new-var
;;      simple-op?
;;      serious?
;;      trivial?
;;      extend-group
;;----------------------------------------------------------------------

(ns ctco.util
  (:require [ctco.expr
             simple]
            [ctco.protocol :as proto])
  (:import  [ctco.expr.simple
             Simple]))

(defn new-var
  "Returns a unique variable for use in the TCO compiler either with a given
  base symbol or a default base symbol."
  ([base]
     (let [new-var (gensym base)]
       (Simple. new-var)))
  ([]
     (let [base "x"]
       (new-var base))))

(defn simple-op?
  "Predicate returning whether op is a simple, value-returning operator."
  [op]
  (some #{op}
        '(+ - * / mod < <= = >= > and or not
          inc dec zero? true? false? nil?
          instance? fn? type ref ref-set deref
          cons conj with-meta meta)))

(defn serious?
  "Given a Clojure TCO expression (as a record type), returns whether the
  expression is serious with respect to the Danvy first-order one-pass CPS
  transformation."
  [expr]
  (extends? proto/PCpsSrs (type expr)))

(defn trivial?
  "Given a Clojure TCO expression (as a record type), returns whether the
  expression is trivial with respect to the Danvy first-order one-pass CPS
  transformation."
  [expr]
  (extends? proto/PCpsTriv (type expr)))

(defmacro extend-multi
  "Like extend, but takes a set of types/classes which will all be extended with
  the given protocol and method map pairs using the same syntax as defrecord for
  protocol function implementations."
  [atype* & proto+mmap*]
  (loop [proto+mmap* proto+mmap*
         fn-map* []
         ext-body []]
    (if (nil? (seq proto+mmap*))
        `(let ~fn-map* ~@(map (fn [a] `(extend ~a ~@ext-body)) atype*))
        (let [fst (first proto+mmap*)
              nxt (next proto+mmap*)]
          (if (list? fst)
              (let [map-var (gensym "fn-map")]
                (recur nxt
                       (conj fn-map*
                             map-var
                             {(keyword (first fst))
                              (cons 'fn (next fst))})
                       (conj ext-body map-var)))
              (recur nxt
                     fn-map*
                     (conj ext-body fst)))))))
