;;----------------------------------------------------------------------
;; File fn.clj
;; Written by Chris Frisz
;; 
;; Created 30 Mar 2012
;; Last modified 18 Oct 2012
;; 
;; Defines the Fn and FnBody record types for representing 'fn'
;; expressions in the Clojure TCO compiler. The Fn record stores the
;; (optional) name field for an 'fn' expression as well as a set of
;; FnBody records. The FnBody record represents clauses in 'fn'
;; expressions. That is, a given Fn record contains one FnBody record
;; for each overloaded arity of a source-level 'fn' expression.
;;
;; FnBody implements the following protocols:
;;
;;      PCpsTriv:
;;              Applies the CPS transformation to the body expression
;;              and extends the formal parameters list with an
;;              additional 'k' argument for the continuation.
;;
;;              The current implementation returns a function that takes
;;              the symbol used to denote the continuation
;;              argument. This makes the CPS transformation for Fn
;;              easier because it allows for easy reuse of the last
;;              argument for function bodies whose original arities
;;              conflict with the CPS arity of another function body by
;;              allowing the symbol for the final argument to be passed
;;              in. This prevents the need for alpha-renaming the
;;              resulting expression or inserting a 'let' expression in
;;              the generated code to bind the continuation variable
;;              name to the value of the final argument. See the
;;              implementation notes for PCpsTriv/cps-triv for the Fn
;;              record type for more details.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(~fml* ~cmap? ~bexpr*).
;;
;;      PUnRecurify:
;;              Applies unrecurify to each subexpression. Uses the
;;              walk-expr function provided by PWalkable.
;;
;;      PWalkable:
;;              Applies the given function to each body expression,
;;              returning a new FnBody record with the results, along
;;              with the original formals list and condition map.
;;
;; Fn implements the following protocols:
;;
;;      PCpsTriv:
;;              Applies the Davny-style first-order one-pass CPS
;;              algorithm to each FnBody (function body) via the
;;              cps-triv function, which returns a function that takes
;;              the representation for the variable associated with the
;;              continuation argument.
;;
;;              It also inserts new FnBody expressions which allow for
;;              "backwards-compatible" calls to the transformed 'fn'
;;              expression. That is, a body which will initiate the
;;              trampoline computation with the empty continuation so
;;              that code that is not transformed via CTCO can make
;;              procedure calls to functions that are transformed. If
;;              the source-level expression did not include a name for
;;              the 'fn' expression, a unique name is inserted to allow
;;              for the recursive call.
;;
;;              In the case of a FnBody whose CPS arity is unique, the
;;              function returned from cps-triv receives a unique
;;              variable.
;;
;;              In the case of a FnBody whose CPS arity conflicts with
;;              the arity of another FnBody (i.e. the source-level 'fn'
;;              expression was overloaded such that it had arities n and
;;              n+1), the returned function is passed the representation
;;              of the symbol for the last argument of the FnBody whose
;;              arity conflicts with it. The FnBody for the CPS version
;;              of the first FnBody and the one for the
;;              backwards-compatible call to the next one are merged by
;;              inserting an 'if' expression that executes the CPS code
;;              if the last argument is a contination (see cont.clj for
;;              how continuations are typed at runtime) and makes the
;;              initial trampoline call to the next FnBody otherwise.
;;
;;              Note that the algorithm presented here is thought to be
;;              its first implementation, having been devised for CTCO.
;;
;;      PUnparse:
;;              Unparses (recursively) the expression as
;;              `(fn ~name? ~bexpr*)
;;
;;      PUnRecurify:
;;              Applies unrecurify to each subexpression, using the name
;;              field of the Fn record if available. Uses the walk-expr
;;              function provided by PWalkable.
;;
;;      PWalkable:
;;              Applies the given function to each FnBody, returning a
;;              new Fn with the results and the same name.
;;
;; FnBody and Fn have the same implementations for the following
;; protocols:
;;
;;      PLoadTrampoline:
;;              Applies load-tramp to each subexpression (with the given
;;              trampoline function name) and returns a new FnBody or Fn
;;              using the walk-expr function provided by PWalkable.
;;
;;      PThunkify:
;;              Applies thunkify to each subexpression and returns a new
;;              FnBody or Fn using the walk-expr function provided by
;;              PWalkable.
;;----------------------------------------------------------------------

(ns ctco.expr.fn
  (:require [ctco.expr
             app cont do if simple thunk tramp]
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
            Thunk]
           [ctco.expr.tramp
            Tramp TrampMark]))

(defrecord FnBody [fml* cmap bexpr*]  
  proto/PCpsTriv
  (cps-triv [this]
    (fn [k]
      (FnBody. (conj (:fml* this) k)
               (:cmap this)
               (mapv
                #(if (util/trivial? %)
                     (AppCont. k (proto/cps-triv %))
                     (proto/cps-srs % k))
                (:bexpr* this)))))

  proto/PUnparse
  (unparse [this]
    `(~(mapv proto/unparse (:fml* this))
      ~@(let [cmap (:cmap this)]
          (if cmap (list cmap) '()))
      ~@(map proto/unparse (:bexpr* this))))

  proto/PUnRecurify
  (unrecurify [this name]
    (proto/walk-expr this #(proto/unrecurify % name) nil))

  proto/PWalkable
  (walk-expr [this f _]
    (FnBody. (:fml* this) (:cmap this) (mapv f (:bexpr* this)))))

(defrecord Fn [name body*]
  proto/PCpsTriv
  (cps-triv [this]
    (let [body* (sort-by (comp count :fml*) (:body* this))]
      (if (nil? (seq body*))
          this
          (let [body (first body*)
                name (or (:name this) (util/new-var "fn"))]
            (letfn [(make-cps-app [body]
                      (with-meta
                        (TrampMark.
                         (App. name
                               (conj (:fml* body)
                                     (let [x (util/new-var "x")]
                                       (Cont. x x)))))
                        {:tramp-entry true}))
                    (make-entry-body [body]
                      (FnBody. (:fml* body)
                               (:cmap body)
                               [(make-cps-app body)]))]
              (loop [body* (next body*)
                     prev-cmp (make-entry-body body)
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
                                       (make-cps-app body))]))
                           (proto/cps-triv body)
                           (conj out prev-cmp))
                          (recur
                           (next body*)
                           (make-entry-body body)
                           (proto/cps-triv body)
                           (conj out
                                 prev-cmp
                                 (prev-cps-fn (util/new-var "k")))))))))))))

  proto/PUnparse
  (unparse [this]
    (let [name (:name this)]
      `(fn ~@(if name (list (proto/unparse name)) '())
         ~@(map proto/unparse (:body* this)))))

  proto/PUnRecurify
  (unrecurify [this name]
    (let [NAME (or (:name this) name)]
      (proto/walk-expr this #(proto/unrecurify % name) nil)))

  proto/PWalkable
  (walk-expr [this f _]
    (Fn. (:name this) (mapv f (:body* this)))))

(util/extend-multi (FnBody Fn)
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))

  proto/PThunkify
  (thunkify [this] (proto/walk-expr this proto/thunkify nil)))
