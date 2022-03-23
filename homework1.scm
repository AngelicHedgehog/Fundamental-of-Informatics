"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define (day-of-week d m y)
  (let ((y (if (< m 3) (- y 1) y))
        (m (if (< m 3) (+ m 10) (- m 2))))
    (remainder
     (+
      d
      (quotient (* 31 m) 12)
      y
      (quotient y 4)
      (- (quotient y 100))
      (quotient y 400)) 7)))

(day-of-week 04 12 1975); ⇒ 4
(day-of-week 04 12 2006); ⇒ 1
(day-of-week 29 05 2013); ⇒ 3

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (calc a b c)
  (let ((D (- (* b b) (* 4 a c))))
    (cond ((< D 0) '())
          ((= D 0) (list (/ (- b) 2 a)))
          (else (list (/ (+ (sqrt D) b) -2 a)
                      (/ (- (sqrt D) b) 2 a))))))

(calc 1 1 1);   ⇒ ()
(calc 1 2 1);   ⇒ (-1)
(calc 1 6 8);   ⇒ (-4 -2)

"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define (my-gcd a b) (if (= b 0) a (my-gcd b (modulo a b))))
(define (my-lcm a b) (* (quotient a (my-gcd a b)) b))
(define (prime? n)
  (letrec ((lower
           (lambda (m)
             (and (not (= 0 (modulo n m)))
                  (or (= m 2) (lower (- m 1)))))))
    (lower (floor (sqrt n)))))

(my-gcd 3542 2464); ⇒ 154
(my-lcm 3 4);       ⇒  12
(prime? 11);        ⇒  #t
(prime? 12);        ⇒  #f