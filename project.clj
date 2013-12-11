;;----------------------------------------------------------------------
;; File project.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified  3 Sep 2013
;; 
;; Project declaration for clojure-tco. 
;;----------------------------------------------------------------------

(defproject chrisfrisz.com/ctco "0.5.0"
  :description "Improving Clojure's support for constant-space tail calls."
  :url "https://github.com/cjfrisz/clojure-tco"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure
                  "1.5.0-alpha3"]
                 [org.clojure/core.match "0.2.0-rc5"]]
  :repl-options {:init-ns ctco.core
                 :init (println "It's TCO time, boys and girls")})
