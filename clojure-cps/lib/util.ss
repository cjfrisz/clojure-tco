;;----------------------------------------------------------------------
;; File util.ss
;; Written by Chris Frisz
;; 
;; Created 15 Jan 2012
;; Last modified 16 Jan 2012
;; 
;; Provides basic utilities helpful for writing CPSed programs.
;;----------------------------------------------------------------------

(library (lib util)

  (export new-var reset-var-num)

  (import (chezscheme))

;; Use an essentially global counter to ensure that each variable is
;; unique.
(define var-num (make-parameter 0))

(define (reset-var-num)
  (var-num 0))

;; The new-var procedure does the actual work of generating the
;; unique variables. It takes a symbol and appends the current value
;; of var-num to the symbol (separated by a '.') to generate the
;; variable. This method is based on the method used to generate
;; unique symbols for CSCI-P423
(define new-var
  (lambda (sym)
    (let ([new-var (string->symbol 
                     (string-append (symbol->string sym) 
                       "$"
                       (number->string (var-num))))])
      (begin
        (var-num (add1 (var-num)))
        new-var))))

) ;; End of library (lib util)