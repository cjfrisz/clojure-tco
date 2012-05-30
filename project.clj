;;----------------------------------------------------------------------
;; File project.clj
;; Written by Chris Frisz
;; 
;; Created  4 Feb 2012
;; Last modified 28 Apr 2012
;; 
;; Project declaration for clojure-tco. 
;;----------------------------------------------------------------------

(defproject ctco "0.2.0"
  :description "Improving Clojure's support for constant-space tail calls."
  :url "https://github.com/cjfrisz/clojure-tco.git"
  :dependencies [[org.clojure/clojure
                  "1.3.0"]
                 [org.clojure/core.match
                  "0.2.0-alpha9"]]
  :dev-dependencies [[swank-clojure
                      "1.4.2"]
                     [vimclojure/server
                      "2.3.1"]]
  :plugins [[lein-swank "1.4.3"]]
  :source-path "src/main"
  :test-path "src/test"
  :repl-init ctco)

