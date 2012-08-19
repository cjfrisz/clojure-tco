;;----------------------------------------------------------------------
;; File let.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified  5 Aug 2012
;; 
;; Defines the LetSrs, LetTriv, and LetCps record types representing serious,
;; trivial, and CPSed 'let' expressions, respectively. LetSrs and LetTriv
;; correspond to 'let' expression that have subexpressions (either the
;; right-hand side of a variable binding or the body) which are "serious" or
;; "trivial" with respect to the Danvy-style CPS algorithm. LetCps corresponds
;; to a 'let' expression that has undergone the CPS transformation.
;;
;; LetCps implements the following protocols:
;;
;;      PAbstractK:
;;              Maps abstract-k over the init values of each binding and the
;;              'let' body.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(let ~bind* ~body)
;;
;;      PThunkify:
;;              Maps thunkify over the init values of each binding and the
;;              'let' body.
;;
;;      PWalkable:
;;              Maps the given function over the init values of each binding and
;;              the 'let' body.
;;
;; LetSrs implements the following protocols:
;;
;;      PCpsSrs:
;;              Applies the CPS transformation to the 'let' expression with
;;              respect to the evaluation continuation. Essentially, for each
;;              "serious" init value, it is pulled out and evaluated before the
;;              remainder of the 'let' expression. Given the static scoping
;;              rules of 'let,' we can apply a slight optimization in the
;;              generated continuation for a "serious" init value, as it need
;;              not use a fresh variable and then perform an unnecessary binding
;;              to the original variable name. Rather, we can simply reuse the
;;              original binding name as the parameter name to the continuation.
;;
;;              For each "trivial" init value, the CPS transformation is appplied
;;              and the result is emitted with its original binding in the order
;;              in which it appeared in the 'let' expression.
;;
;;              For example, consider the following expression:
;;
;;			(let [x ((fn [y] y) 5)
;;			      z 3]
;;			  (+ x z)
;;
;;              A naive transformation might do something like this:
;;
;;			((fn [y k4354] (k4354 y))
;;			 5
;;			 (fn [k4355]
;;			   (let [x k4355
;;			         z 3]
;;			     (k4353 (+ x z)))))
;;
;;              By using the optimization described above, we instead get the
;;              following:
;;
;;			((fn [y k4354] (k4354 y))
;;			 5
;;			 (fn [x]
;;			   (let [z 3]
;;			     (k4353 (+ x z)))))
;;
;;              Additionally, the transformation elides empty binding vectors,
;;              such as (let [] 5), instead emitting only the body, i.e. 5.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(let ~bind* ~body)
;;
;; LetTriv implements the following protocols:
;;
;;      PCpsTriv:
;;              Maps cps-triv over the init values for each binding as well as
;;              the body of the 'let' expression.
;;
;;      PEmit:
;;              Emits (recursively) the syntax for the expression as
;;              `(let ~bind* ~body)
;;
;;      PWalkable:
;;              Maps the given function over the init values of each binding and
;;              the 'let' body.
;;
;;----------------------------------------------------------------------

(ns ctco.expr.let
  (:require [ctco.expr
             cont]
            [ctco.protocol :as proto]
            [ctco.util :as util])
  (:import [ctco.expr.cont
            AppCont Cont]))

(defrecord LetCps [bind* body]
  proto/PAbstractK
    (abstract-k [this app-k]
      (let [ctor #(LetCps. %1 %2)]
        (proto/walk-expr this #(proto/abstract-k % app-k) ctor)))

  proto/PThunkify
    (thunkify [this]
      (let [ctor #(LetCps. %1 %2)]
        (proto/walk-expr this proto/thunkify ctor))))

(defrecord LetSrs [bind* body]
  proto/PCpsSrs
    (cps-srs [this k]
      (letfn [(build-let [bind* body]
                (if (not (nil? (seq bind*)))
                    (LetCps. bind* body)
                    body))
              (cps-let [bind-in* bind-out*]
                (if (nil? (seq bind-in*))
                    (let [body (:body this)
                          BODY (if (util/trivial? body)
                                   (let [arg (proto/cps-triv body)]
                                     (AppCont. k arg))
                                   (proto/cps-srs body k))]
                      (build-let bind-out* BODY))
                    (let [var (first bind-in*)
                          init (fnext bind-in*)
                          BIND-IN* (nnext bind-in*)]
                      (if (util/trivial? init)
                          (let [INIT (proto/cps-triv init)
                                BIND-OUT* (conj bind-out* var INIT)]
                            (recur BIND-IN* BIND-OUT*))
                          (let [k-body (cps-let BIND-IN* [])
                                K (Cont. var k-body)
                                let-body (proto/cps-srs init K)]
                            (build-let bind-out* let-body))))))]
        (cps-let (:bind* this) []))))

(defrecord LetTriv [bind* body]
  proto/PCpsTriv
    (cps-triv [this]
      (let [ctor #(LetCps. %1 %2)]
        (proto/walk-expr this proto/cps-triv ctor))))

(def let-emit
  {:emit (fn [this]
           (let [bind* (vec (map proto/emit (:bind* this)))
                 body (proto/emit (:body this))]
             `(let ~bind* ~body)))})

(def let-walkable
  {:walk-expr (fn [this f ctor]
                (letfn [(walk-bind* [bind-in* bind-out*]
                          (if (nil? (seq bind-in*))
                              bind-out*
                              (let [var (first bind-in*)
                                    init (fnext bind-in*)
                                    INIT (f init)
                                    BIND-IN* (nnext bind-in*)
                                    BIND-OUT* (conj bind-out* var INIT)]
                                (recur BIND-IN* BIND-OUT*))))]
                  (let [BIND* (walk-bind* (:bind* this) [])
                        BODY (f (:body this))]
                    (ctor BIND* BODY))))})

(extend LetCps
  proto/PEmit
    let-emit

  proto/PWalkable
    let-walkable)

(extend LetSrs
  proto/PEmit
    let-emit)

(extend LetTriv
  proto/PEmit
    let-emit

  proto/PWalkable
    let-walkable)
