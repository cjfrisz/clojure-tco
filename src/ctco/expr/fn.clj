;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified  2 Oct 2012
;; 
;; Defines the FnBody record type for representing 'fn' expressions in the
;; Clojure TCO compiler.
;;
;; It implements the following protocols:
;;
;;      PAbstractK:
;;              Recursively applies abstract-k to the body expression,
;;              returning a new FnBody record.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(fn ~fml* body).
;;
;;      PCpsTriv:
;;              Applies the CPS transformation to the body expression
;;              and extends the formal parameters list with an
;;              additional 'k' argument for the continuation.
;;
;;      PThunkify:
;;              Simply calls thunkify on the body and returns a new FnBody
;;              record with that body value. 
;;----------------------------------------------------------------------

(ns ctco.expr.fn
  (:require [ctco.expr
             app cont do if simple thunk]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.app
            App]
           [ctco.expr.cont
            Cont AppCont]
           [ctco.expr.do
            Do]
           [ctco.expr.if
            IfCps]
           [ctco.expr.simple
            Simple]
           [ctco.expr.thunk
            Thunk]))

(defrecord FnBody [fml* cmap bexpr*]
  proto/PAbstractK
  (abstract-k [this app-k]
    (proto/walk-expr this #(proto/abstract-k % app-k) nil))

  proto/PUnparse
  (unparse [this]
    `(~(reduce #(conj %1 (proto/unparse %2)) [] (:fml* this))
      ~@(let [cmap (:cmap this)]
          (if cmap (list cmap) '()))
      ~@(map proto/unparse (:bexpr* this))))
  
  proto/PCpsTriv
  (cps-triv [this]
    ;; returning a function this way saves us a 'let' binding in the generated
    ;; code and is easily managed by the fact that operations on a FnBody are
    ;; only ever invoked from a Fn.
    (fn [k]
      (FnBody. (conj (:fml* this) k)
               (:cmap this)
               (reduce (fn [e* e]
                         (conj e*
                               (condp extends? (type e)
                                 proto/PCpsTriv (AppCont. k (proto/cps-triv e))
                                 proto/PCpsSrs (proto/cps-srs e k))))
                       []
                       (:bexpr* this)))))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
  (walk-expr [this f _]
    (FnBody. (:fml* this)
             (:cmap this)
             (reduce (fn [e* e] (conj e* (f e))) [] (:bexpr* this)))))

(defrecord Fn [name body*]
  proto/PAbstractK
  (abstract-k [this app-k]
    (proto/walk-expr this #(proto/abstract-k % app-k) nil))

  proto/PCpsTriv
  (cps-triv [this]
    (let [body* (sort-by (comp count :fml*) (:body* this))]
      (if (nil? (seq body*))
          this
          (let [body (first body*)
                name (or (:name this) (util/new-var "fn"))]
            (letfn [(make-cps-app [body]
                      (App. name
                            (conj (:fml* body)
                                  (let [x (util/new-var "x")]
                                    (Cont. x x)))))
                    (make-compat-body [body]
                      (FnBody. (:fml* body)
                               (:cmap body)
                               [(make-cps-app body)]))]
              (loop [body* (next body*)
                     prev-cmp (make-compat-body body)
                     prev-cps-fn (proto/cps-triv body)
                     out []]
                (if (nil? (seq body*))
                    (Fn. name (conj out
                                    prev-cmp
                                    (prev-cps-fn (util/new-var "k"))))
                    (let [body (first body*)
                          body-fml* (:fml* body)]
                      (if (= (count body-fml*) (inc (count (:fml* prev-cmp))))
                          (recur
                           (next body*)
                           (let [last-fml (last body-fml*)]
                             (FnBody.
                              body-fml*
                              ;; NB: need to revisit this when
                              ;; NB: providing better support for
                              ;; NB: condition maps. could include
                              ;; NB: references to the variable we're
                              ;; NB: co-opting for the continuation.
                              (:cmap body)
                              [(IfCps. (App. (Simple. ':kont)
                                             [(App. (Simple. 'meta)
                                                    [last-fml])])
                                       (Do.
                                        (:bexpr* (prev-cps-fn last-fml)))
                                       (with-meta (make-cps-app body)
                                         {:tramp-entry true}))]))
                           (proto/cps-triv body)
                           (conj out prev-cmp))
                          (recur
                           (next body*)
                           (make-compat-body body)
                           (proto/cps-triv body)
                           (conj out
                                 prev-cmp
                                 (prev-cps-fn (util/new-var "k")))))))))))))

  proto/PUnparse
  (unparse [this]
    (let [name (:name this)]
      `(fn ~@(if name (list (proto/unparse name)) '())
         ~@(map proto/unparse (:body* this)))))

  proto/PThunkify
  (thunkify [this]
    (proto/walk-expr this proto/thunkify nil))

  proto/PWalkable
  (walk-expr [this f _]
    (Fn. (:name this)
         (reduce (fn [b* b] (conj b* (f b))) [] (:body* this)))))
