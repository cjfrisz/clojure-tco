(include "pmatch.scm")

(define var-num (make-parameter 0))

(define (reset-var-num)
  (var-num 0))

(define new-var
  (lambda (sym)
    (let ([new-var (string->symbol 
                     (string-append (symbol->string sym) 
                       "."
                       (number->string (var-num))))])
      (begin
        (var-num (add1 (var-num)))
        new-var))))

(define trivial?
  (lambda (t)
    (pmatch t
      [,t (guard (symbol? t)) #t]
      [(lambda ,fmls ,body) #t]
      [else #f])))

(define (verify-lambda-calc e)
  (pmatch e
    [,x (guard (symbol? x)) #t]
    [(lambda (,fst . ,rst) ,body) (verify-lambda-calc body)]
    ;; We make a special case for applying lambda expressions to
    ;; verify an appropriate number of arguments.
    [((lambda ,fmls ,body) ,a1 . ,ar)
     (if (= (length `(,a1 . ,ar)) (length fmls))
         (and (verify-lambda-calc body)
              (for-each verify-lambda-calc `(,a1 . ,ar)))
         (errorf 'verify-lambda-calc
           "Mismatched number of formals and arguments in ~s"
           e))]
    [(,rator . ,rand) (and (verify-lambda-calc rator)
                           (for-each verify-lambda-calc rand))]
    [else (errorf 'verify-lambda-calc
            "Invalid lambda calculus expression ~s"
            e)]))


(define (returnK e)
  (lambda (k)
    (k e)))

(define (bindK ma next)
  (lambda (k)
    (let ([k^ (lambda (a)
                (let ([mb (next a)])
                  (mb k)))])
      (ma k^))))

(define-syntax letMK
  (syntax-rules ()
    [(_ ((name init)) expr)
     (bindK init (lambda (name) expr))]))

(define-syntax letMK*
  (syntax-rules ()
    [(_ ((name init)) expr) 
     (letMK ((name init)) expr)]
    [(_ ((name1 init1) (name2 init2) ...) expr)
     (letMK ([name1 init1]) 
       (letMK* ([name2 init2] ...) expr))]))

(define (E e k)
  (if (trivial? e)
      `(,k ,(T e))
      (S e k))) 


;; (define (S e ks k)
;;   (if (null? e)
;;       (k ks)
;;       (let ([fst (car e)] [rst (cdr e)])
;;         (cond
;;           [(trivial? fst)
;;            (let ([fst^ (T fst)])
;;              (S rst ks (lambda (r) `(,(k fst^) ,r))))]
;;           [else
;;             (let ([s (new-var 's)])
;;               (let ([rst^ (S e^ s k)])
;;                 (S fst ks `(lambda (,s) ,rst^))))]))))

(define (S e k)
  (define (inner-k e k)
    (define (simple-call e)
      (if (null? e)
          (values `(,k) '())
          (let ([fst (car e)])
            (let-values ([(rst sym) (simple-call (cdr e))])
              (if (trivial? fst)
                  (values (cons (T fst) rst) sym)
                  (let ([s (new-var 's)])
                    (let ([rst^ (cons s rst)])
                      (if (null? sym)
                          (values rst^ s)
                          (values rst^ sym)))))))))
    (let-values ([(expr sym) (simple-call e)])
      `(lambda (,sym) ,expr)))
  (if (null? e)
      `(,k)
      (let ([k^ (inner-k e k)]
            [e-rev (reverse e)])
        (let ([fst (car e-rev)] [rst (cdr e-rev)])
          (if (trivial? fst)
              (cons (T fst) (S rst k^))
              (let ([rst^ (S rst k^)])))))))

(define (T e)
  (pmatch e
    [,x (guard (symbol? x)) x]
    [(lambda ,fmls ,body)
     (let ([k (new-var 'k)])
       (let ([fmls^ (append fmls `(,k))])
         `(lambda ,fmls^ ,(E body k))))]))

(define (cps e)
  (begin
    (verify-lambda-calc e)
    (reset-var-num)
    (let ([k (new-var 'k)])
      `(lambda (,k) ,(E e k)))))