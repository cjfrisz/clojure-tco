;;----------------------------------------------------------------------
;; File protocol.clj
;; Written by Chris Frisz
;; 
;; Created 26 Apr 2012
;; Last modified 13 Sep 2012
;; 
;; Includes the protocols used in the CTCO compiler. These include the
;; following:
;;
;; PAbstractK:
;;      Implemented by expressions for abstracting over the
;;      representation of continuations.
;;
;; PCpsSrs:
;;      Implemented by expressions which can undergo a CPS
;;      transformation and are considered "serious." As a rule of
;;      thumb, "serious" expressions are either function application
;;      or expressions that include a function application as a
;;      subexpression.
;;
;; POverload:
;;      Implemented by expressions for properly overloading 'fn'
;;      expressions so that they have entrypoints for both CPS and
;;      non-CPS versions.
;;
;; PCpsTriv:
;;      Implemented by expressions which can undergo a CPS
;;      transformation and are considered "trivial." As a rule of
;;      thumb, this includes simple expressions (numbers, booleans,
;;      etc.) and non-function application expressions that contain
;;      no serious subexpressions.
;;
;; PUnparse:
;;      Implemented by expressions that need to be unparseted as code
;;      from the intermediate representation used in CTCO.
;;
;; PThunkify:
;;      Implemented by expressions for "thunkification," or the
;;      process of ensuring a recursive function periodically returns
;;      a function of no arguments, the basis for trampolining.
;;
;; PWalkable:
;;      Implemented by expressions for which a new expression needs to
;;      be created by applying a function to each of its
;;      subexpressions. This can be thought of as "map" for
;;      heterogeneously-shaped expressions. It's also generally only
;;      used internally for limiting code reuse for expression
;;      traversals in CTCO passes.
;;----------------------------------------------------------------------

(ns ctco.protocol)

(defprotocol PAbstractK
  "Defines the 'abstract-k' function which abstracts over continutation
  application using the given symbol to represent the function that implements
  continutation appliction."
  (abstract-k [this app-k]
    "Abstracts over continutation application in an expression by converting
    continuation applications to calls to app-k."))

(defprotocol PCpsSrs
  "Protocol for applying the CPS transformation to serious expressions (a la
  Olivier)."
  (cps-srs [this k]
    "Applies the CPS transformation for serious expressions with respect to the
    Danvy-style CPS algorithm."))

(defprotocol POverload
  "Protocol for overloading 'fn' expressions to have CPS and non-CPS
  entrypoints."
  (overload [this]
    "Recursively overloads all 'fn' expressions contained within the
    expression.")) 

(defprotocol PCpsTriv
  "Protocol for applying the CPS transformations to trivial expressions (a la
  Danvy)."
  (cps-triv [this]
    "Applies the CPS transformation for serious expressions with respect to the
    Olivier-style CPS algorithm."))

(defprotocol PUnparse
  "Protocol for TCO expressions that can be represented as a sequence."
  (unparse [this]
    "Unparses a sequence representing the Clojure syntax for the TCO 
    expression."))

(defprotocol PThunkify
  "Protocol for expressions that can be thunkified in the TCO compiler."
  (thunkify [this]
    "Transform all functions to return thunks."))

(defprotocol PWalkable
  "Protocol for TCO expressions that can be walked."
  (walk-expr [this f ctor]
    "Applies the function f to the subforms of the argument expression,
    returning an expression created with the constructor."))
