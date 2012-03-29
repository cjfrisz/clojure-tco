;;----------------------------------------------------------------------
;; File tco_pass.clj
;; Written by Chris Frisz
;; 
;; Created 28 Mar 2012
;; Last modified 28 Mar 2012
;;
;; Defines the base protocol and record type used by the Clojure TCO
;; compiler.
;;----------------------------------------------------------------------

(ns clojure-tco.tco-pass)

(defprotocol PTcoPass
  "Protocol implemented by passes in the Clojure TCO compiler."
  (register [this form handler]
    "Registers the language form as using the handler function for a TCO pass.")
  (register-default [this default]
    "Registers a default handler to use when an expression doesn't match any
    registered language forms.")
  (run [this expr]
    "Given a language form, runs the handler for that form on the expression,
    returning the transformed expression."))

(defrecord TcoPass [form->handler]
  PTcoPass
  (register [this form handler]
    (assoc (:form->handler this) form handler))
  (register-default [this default]
    (register this :default default))
  (run [this expr]
    (let [form (or (some #(when (instance? % expr) (keys this)))
                   :default)
          handler ((:form->handler this) form)]
      (handler expr))))
