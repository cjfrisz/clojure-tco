;; Originally
(((lambda (x) (lambda (y) x)) a) b)

;; Currently
(let ([a 3])
    ((lambda (k.0)
       (((lambda (x k.2) (k.2 x))
         (lambda (y k.3) (k.3 y))
         k.0)
        (lambda (s.1) (s.1 a k.0)) k.0))
     (lambda (x) x)))

;; Ideally
(let ([a 3])
    ((lambda (k.0)
       ((lambda (x k.2) (k.2 x))
         (lambda (y k.3) (k.3 y))
        (lambda (s.1) (s.1 a k.0))))
     (lambda (x) x)))