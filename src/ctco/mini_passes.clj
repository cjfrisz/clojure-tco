;;----------------------------------------------------------------------
;; File mini_passes.clj
;; Written by Chris Frisz
;; 
;; Created 14 Apr 2012
;; Last modified  5 Oct 2012
;; 
;; Defines the small, one-time code transformations for the TCO
;; compiler. These include the following:
;;      overload
;;      make-apply-k
;;      make-trampoline
;;----------------------------------------------------------------------

(ns ctco.mini-passes
  (:use [clojure.core.match
         :only (match)]))

(defn make-trampoline
  "Introduces the definition of the trampoline function for expr using tramp as
  the name of the function.

  The function is let-bound, keeping it locally-scoped to expr when the
  expression is emitted."
  [expr tramp]
  (let [thunk (gensym 'thunk)]
    `(letfn [(~tramp [~thunk]
               (if (get (meta ~thunk) :thunk)
                   (recur (~thunk))
                   ~thunk))]
       ~expr)))
