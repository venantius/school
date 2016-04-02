(define (square x)
 (* x x))

(define (sum-of-squares x y)
 (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (>= x y)
  (or (> x y) (= x y)))

;; exercise 1.1
10 ;; 10
(+ 5 4 3) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; a
(define b (+ a 1)) ;; b
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> a b) (< b (* a b)))
  b
  a) ;; 3
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
   (* 3 (- 6 2) (- 2 7))) ;; -23/90

;; exercise 1.3
(define (one-three a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        (else (sum-of-squares b c))))

;; exercise 1.4
;; the function provided here checks to see if b is greater than 0; if so, it
;; sets the function in the operator position to `+`, otherwise `-`. In effect,
;; this means that if b is less than zero we do a - b. In the base case we do
;; a + b

;; exercise 1.5
;; I still find this question confusing. I've always had to look up the answer
;; to understand it.

;; The ostensible answer is that under applicative-order evaluation, this form
;; will never complete, because it ends up recursively evaluating to itself. By
;; constrast, the normal order form ends up fully expanding everything else, and
;; therefore reaches a point at which the `if` form realizes it does not need
;; to evaluate (p).

;; 1.1.7: Example: Square Roots by Newton's Method

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

#|
What happens here is that both guess and sqrt-iter are evaluated before being
passed to new-if, because new-if is not a special form (even though it contains
cond). As a result, sqrt-iter ends up being called infinitely.
#|

;; Exercise 1.7
