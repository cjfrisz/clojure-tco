;;----------------------------------------------------------------------
;; File recur.clj
;; Written by Chris Frisz
;; 
;; Created 16 Apr 2012
;; Last modified 19 Oct 2012
;; 
;; Defines the Recur record type and operations for representing
;; 'recur' expressions in the Clojure TCO compiler.
;;
;; Recur implements the following protocols:
;;
;;      PLoadTrampoline:
;;              Maps load-tramp over the arguments to the 'recur' form.
;;
;;      PUnparse:
;;              Unparses (recursively) the syntax for the expression as
;;              `(loop ~bind* ~body), where bind* is the vector of
;;              variables and bindings, and body is the body
;;              expression of the 'loop.'
;;
;;      PUnRecurify:
;;              Replaces the 'recur' call with a function application.
;;
;;      PWalkable:
;;              Maps a function over the arguments to the 'recur' form,
;;              generating a new Recur record.
;;----------------------------------------------------------------------

(ns ctco.expr.recur
  (:require [ctco.expr.app]
            [ctco.protocol :as proto])
  (:import [ctco.expr.app
            App]))

(defrecord Recur [arg*]
  proto/PLoadTrampoline
  (load-tramp [this tramp]
    (proto/walk-expr this #(proto/load-tramp % tramp) nil))

  proto/PUnparse
  (unparse [this]
    (let [arg* (map proto/unparse (:arg* this))]
      `(recur ~@arg*)))

  proto/PUnRecurify
  (unrecurify [this name]
    (App. name (:arg* this)))

  proto/PWalkable
  (walk-expr [this f _]
    (Recur. (mapv f (:arg* this)))))
