;;----------------------------------------------------------------------
;; File project.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified  9 Feb 2012
;; 
;; Project declaration for clojure-tco. 
;;----------------------------------------------------------------------

(defproject clojure-tco "0.1.1"
  :description "Adding proper tail calls to Clojure."
  :url "https://github.iu.edu/cjfrisz/clojure-tco.git"
  :dependencies [[org.clojure/clojure
                  "1.3.0"]
                 [org.clojure/core.match
                  "0.2.0-alpha9"]
                 [com.id8/algo.monads
                  "0.1.2"]]
  :source-path "src/main/"
  :repl-init clojure-tco.cps)