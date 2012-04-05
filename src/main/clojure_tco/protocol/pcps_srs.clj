;;----------------------------------------------------------------------
;; File pcps-srs.clj
;; Written by Chris Frisz
;; 
;; Created  5 Apr 2012
;; Last modified  5 Apr 2012
;; 
;; Defines the PCpsSrs protocol for applying the Olivier-style CPS
;; transformation to "serious" Clojure expression record types.
;;----------------------------------------------------------------------

(ns clojure-tco.protocol.pcps-srs)

(defprotocol PCpsSrs
  "Protocol for applying the CPS transformation to serious expressions (a la
  Olivier)."
  (cps [this k]
    "Applies the CPS transformation for serious expressions with respect to the
    Olivier-style CPS algorithm."))
