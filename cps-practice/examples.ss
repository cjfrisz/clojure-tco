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

;; Originally
(((lambda (x) x) (lambda (y) y)) (((lambda (u) (lambda (v) u)) a) b))

;; Currently
(lambda (k.0)
  ((lambda (x k.7) (k.7 x))
    (lambda (y k.6) (k.6 y))
    (lambda (s.1)
      (s.1 (lambda (u k.4) (k.4 (lambda (v k.5) (k.5 u))))
           a
           (lambda (s.3) (s.3 b (lambda (s.2) (s.2 k.0))))))))

;; Ideally
(lambda (k.0)
  ((lambda (x k.1) (k.1 x))
   (lambda (y k.2) (k.2 y))
   (lambda (s.1)
     (s.1 ((lambda (u k.3) (k.3 (lambda (v k.4) (k.4 u))))
           a
           (lambda (s.2) (s.2 b k.0)))
       k.0))))