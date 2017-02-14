#lang racket
(define (square x)
   (* x x))

(define (sum-of-squares x y)
   (+ (square x)(square y)))
(define (f a)
   (sum-of-squares(+ a 1) (* a 2)))
; 条件表达式和谓词
(define (abs x)
  (cond ((> x 0 )x)
        ((= x 0) 0)
        ((< x 0) (- x))
        ))
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)
   ))
(define (abs3 x)
  (if (< x 0)
        (- x)
        x
        ))
(f 5)
(abs -3)
(abs2 -3)
(abs3 -3)
; 定义大于等于,重载,内部实际定义了大于等于
(define (>= x y)
  (or (> x y) (= x y)))
(>= 5 3)
(>= 1 3)


; exercise 1.1
"exercise 1.1"
10
(+ 5 3 4)   ;12
(- 9 1)   ;8
(/ 6 2)  ;3
(+ (* 2 4) (- 4 6)) ;6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))  ;19
(= a b)     ;#f
(if (and (> b a ) (< b (* a b)))
    b
    a)  ;4
(cond ( (= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)
       )    ;16

(+ 2 (if (> b a) b a))  ;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)
   )    ;16


;exercise 1.2
"exercise 1.2"
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4.0 5)))) )
   (* 3 (- 2 6) (- 2 7)))

"exercise 1.3"


(define (min x y z)
  (cond ((and (<= x y) (<= x z))  x)
        ((and (<= y x) (<= y z))   y)
        (else z)
        ))
(define (sumbig2 x y z)
  (- (+ x y z) (min x y z)))
(sumbig2 3 5 6)

"exercise 1.4"
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 4)
(a-plus-abs-b 3 -4)

"exercise 1.5"
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))

"title 1.1.7"
;改进猜测的方式就是求出它与被开方数除以上一个猜测的平均值
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x) ) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))

(sqrt (+ (sqrt 2) (sqrt 3)))

(square (sqrt 1000))

"exercise 1.6"
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x) x)))
(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

;(sqrt2 9)

"exercise 1.7"
(define (is-small-part-of? small-one big-one)
  (< (abs (/ small-one big-one)) 0.001))
(define (sqrt-iter3 guess x previous-guess)
  (if (is-small-part-of? (- previous-guess guess) previous-guess )
      guess
      (sqrt-iter3 (improve guess x) x guess))
  )
;没有上一次猜测时，选择一个和1相差比较大的数即可
(sqrt-iter3 1.0 0.00000016 0.00000016)


"exercise 1.8"
(define (cube x)
  (* x x x))
(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x )) 0.0001))
(define (improve-cube y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cube-root guess x)
  (if (good-enough-cube? guess x)
  guess
  (cube-root (improve-cube guess x) x)
  ))
(define (cube-root-cal x)
  (cube-root 1.0 x))
(cube-root-cal 3)

;title 1.1.8
(define (mysqrt x)
  (define (good-enough? guess)
     (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(mysqrt 9)

;