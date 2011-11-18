;;; Code written by Oleg Kiselyov
;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.

; (pmatch exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- comparison with exp (using equal?)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

; Modified by Adam C. Foltzer for R6RS compatibility

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch v cs ...)))
    ((_ v) (error 'pmatch "failed: ~s" v))
    ((_ v (else e0 e ...)) (begin e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (uscore quote unquote)
    ((_ v uscore kt kf)
     ; _ can't be listed in literals list in R6RS Scheme
     (and (identifier? #'uscore) (free-identifier=? #'uscore #'_))
     kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
