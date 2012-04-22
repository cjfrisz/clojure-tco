;;----------------------------------------------------------------------
;; File pcps-triv.clj
;; Written by Chris Frisz
;; 
;; Created  5 Apr 2012
;; Last modified 22 Apr 2012
;; 
;; Defines the PCpsTriv protocol for applying the Olivier-style CPS
;; transformation to "trivial" Clojure expression record types.
;;----------------------------------------------------------------------

(ns ctco.protocol.pcps-triv)

(defprotocol PCpsTriv
  "Protocol for applying the CPS transformations to trivial expressions (a la
  Olivier)."
  (cps [this]
    "Applies the CPS transformation for serious expressions with respect to the
    Olivier-style CPS algorithm."))
