;;----------------------------------------------------------------------
;; File protocol.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 14 Oct 2012
;; 
;; Includes the protocols used in the CTCO compiler. These include the
;; following:
;;
;; PCpsSrs:
;;      Implemented by expressions which can undergo a CPS
;;      transformation and are considered "serious." As a rule of
;;      thumb, "serious" expressions are either function application
;;      or expressions that include a function application as a
;;      subexpression.
;;
;; PCpsTriv:
;;      Implemented by expressions which can undergo a CPS
;;      transformation and are considered "trivial." As a rule of
;;      thumb, this includes simple expressions (numbers, booleans,
;;      etc.) and non-function application expressions that contain
;;      no serious subexpressions.
;;
;; PLoadTrampoline:
;;      Compiler pass for loading appropriate expressions onto the
;;      trampoline. Currently this only includes expressions marked by
;;      the TrampMark reccord, which indicate the "compatibility" bodies
;;      for 'fn' expressions. That is, function bodies which are
;;      produced to allow non-CTCO code to call into functions
;;      transformed by CTCO.
;;
;; PThunkify:
;;      Implemented by expressions for "thunkification," or the
;;      process of ensuring a recursive function periodically returns
;;      a function of no arguments, the basis for trampolining.
;;
;; PUnparse:
;;      Implemented by expressions that need to be unparseed as code
;;      from the intermediate representation used in CTCO.
;;
;; PUnRecurify:
;;      Removes uses of 'recur' to make CTCO transformations (e.g. CPS)
;;      safe. Replaces calls to 'recur' with direct calls to the
;;      function in which they reside.
;;
;; PWalkable:
;;      Implemented by expressions for which a new expression needs to
;;      be created by applying a function to each of its
;;      subexpressions. This can be thought of as "map" for
;;      heterogeneously-shaped expressions. It's also generally only
;;      used internally for limiting code copying for expression
;;      traversals in CTCO passes.
;;----------------------------------------------------------------------

(ns ctco.protocol)

(defprotocol PCpsSrs
  "Protocol for applying the CPS transformation to serious expressions (a la
  Olivier)."
  (cps-srs [this k]
    "Applies the CPS transformation for serious expressions with respect to the
    Danvy-style CPS algorithm."))

(defprotocol PCpsTriv
  "Protocol for applying the CPS transformations to trivial expressions (a la
  Danvy)."
  (cps-triv [this]
    "Applies the CPS transformation for serious expressions with respect to the
    Danvy-style CPS algorithm."))

(defprotocol PLoadTrampoline
  "Protocol for loading CTCO expressions onto the trampoline appropriately."
  (load-tramp [this tramp]
    "Takes a CTCO record expression and a symbol for the name of the trampoline
    function and loads the proper expressions onto said trampoline."))

(defprotocol PUnparse
  "Protocol for TCO expressions that can be represented as a sequence."
  (unparse [this]
    "Unparses a sequence representing the Clojure syntax for the TCO 
    expression."))

(defprotocol PUnRecurify
  "Protocol for replacing 'recur' forms with direct calls."
  (unrecurify [this name]
    "Replaces 'recur' forms with calls to the function given by 'name.'"))

(defprotocol PThunkify
  "Protocol for expressions that can be thunkified in the TCO compiler."
  (thunkify [this]
    "Transform all functions to return thunks."))

(defprotocol PWalkable
  "Protocol for TCO expressions that can be walked."
  (walk-expr [this f ctor]
    "Applies the function f to the subforms of the argument expression,
    returning an expression created with the constructor."))
