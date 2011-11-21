;; Let's start with the simple, intuitive definition of factorial 
;; This is how we naturally think of factorial being defined, but the
;; multiplication in the tail position necessitates that the callee's 
;; stack frame be preserved until the recursive call to fact returns
;; with the answer to (fact (- n 1))
(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))

;; What's cooler than that? The tail-recursive version:
;; This solves the tail call issue; the subtraction and multiplication
;; in the arguments to the recursive call to fact-tr return quickly,
;; putting the call in tail position. Thanks to Scheme's requirement
;; for tail call optimization, this means that the recursive calls to 
;; fact-tr use no additional stack frames and the memory usage for this
;; is constant. 
(define fact-tr
  (lambda (n acc)
    (if (= n 0)
	acc
	(fact-tr (- n 1) (* acc n)))))

;; Although the above definition is super cool, it's not very natural.
;; It doesn't fit as nicely with our intuitive notion of fact, so we
;; can write a little driver to make it better.
(define fact-tr-driver
  (lambda (n)
    (fact-tr n 1)))

;; Ok, that's fun and all, but what's the best version of factorial?
;; Well it's clearly the CPSed version. By CPSing the function, we 
;; create an explicit continuation and pass it to each iteration of 
;; the function. This has the wonderful effect of making all calls
;; into tail calls. Just watch:
(define fact-cps
  (lambda (n k)
    (if (= n 0)
	(k 1)
	(fact-cps (- n 1) (lambda (v) 
			    (k (* n v)))))))

;; Here's some CPSed functions thanks to C311
(define length
  (lambda (ls)
    (cond
     [(null? ls) 0]
     [else (+ (length (cdr ls)) 1)])))

(define length-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
     [else (length-cps (cdr ls) 1 (lambda (v) (k (+ v 1))))])))

(define binary-to-decimal
  (lambda (n)
    (cond
     [(null? n) 0]
     [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
     [(null? n) (k 0)]
     [else (binary-to-decimal (cdr n) 
			      (lambda (v1) 
				(+ (car n) (* 2 v))))])))

(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (lambda (m)
    (lambda (n)
      (lambda (k)
	(k (+ m n))))))


(define count-syms*
  (lambda (ls)
    (cond
     [(null? ls) 0]
     [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdf ls)))]
     [(symbol? (car ls)) (+ (count-syms* (cdr ls)) 1)]
     [else (count-syms* (cdr ls))])))

(define count-syms*-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
     [(pair? (car ls))
      (count-syms*-cps (car ls) 
		       (lambda (v1)
			 (count-syms*-cps (cdr ls)
					  (lambda (v2)
					    (k (+ v1 v2))))))]
     [(symbol (car ls)) (count-syms*-cps (cdr ls) (lambda (v) (k (+ v 1))))]
     [else (count-syms*-cps (cdr ls) k)])))


(define walk
  (lambda (v ls)
    (cond
     [(symbol? v)
      (let ([p (assq v ls)])
	(cond
	 [p (walk (cdr p) ls)]
	 [else v]))]
     [else v])))

(define walk-cps
  (lambda (v ls k)
    (cond
     [(symbol? v)
      (let ([p (assq v ls)])
	(cond
	 [p (walk (cdr p) ls k)]
	 [else (k v)]))]
     [else (k v)])))
     
(define tree-sum
  (lambda (ls)
    (cond
     [(null? ls) 0]
     [(pair? (car ls))
      (+ (tree-sum (car ls))
	 (tree-sum (cdr ls)))]
     [else (+ (car ls) (tree-sum (cdr ls)))])))

(define tree-sum-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
     [(pair? (car ls))
      (tree-sum (car ls)
		(lambda (v1)
		  (tree-sum (cdr ls)
			    (lambda (v2)
			      (k (+ v1 v2))))))]
     [else (tree-sum (cdr ls) (lambda (v)
				(k (+ (car ls) v))))])))

(define ack
  (lambda (m n)
    (cond
     [(= 0 m) (+ n 1)]
     [(= 0 n) (ack (- m 1) 1)]
     [else (ack (- m 1) (ack m (- n 1)))])))

(define ack-cps
  (lambda (m n k)
    (cond
     [(= 0 m) (k (+ n 1))]
     [(= 0 n) (ack (- m 1) 1 k)]
     [else (ack m (- n 1) (lambda (v)
			    (ack (- m 1) v k))


(define fact
  (lambda (n)
    ((lambda (fact)
       (fact fact n))
     (lambda (fact n)
       (cond
	[(zero? n) 1]
	[else (* n (fact fact (- n 1)))])))))

(define fact-cps
  (lambda (n k)
    ((lambda (fact k1)
       (fact fact n k1))
     (lambda (fact n k2)
       (cond
	[(zero? n) (k2 1)]
	[else (fact fact (- n 1) (lambda (v) (k2 (* n v))))]))
     k)))

(define pascal
  (lambda (n)
    (let ([pascal
	   (lambda (pascal)
	     (cond
	      [(> m n) '()]
	      [else (let ([a (+ a m)])
		       (cons a ((pascal pascal) (+ m 1) a)))]))])
      ((pascal pascal) 1 0))))

(define pascal-cps
  (lambda (n k)
    (let ([pascal
	   (lambda (pascal k)
	     (cond
	      [(> m n) (k '())]
	      [else (let ([a (+ a m)])
		      ((pascal pascal (lambda (v) (k (cons a v))))
		       (+ m 1)
		       a))]))])
      ((pascal pascal k) 1 0))))
