"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define (count x xs)
  (if (null? xs)
      0
      (+
       (count x (cdr xs))
       (if (eqv? x (car xs))
           1
           0))))

(count 'a '(a b c a)); ⇒ 2
(count 'b '(a c d));   ⇒ 0
(count 'a '());        ⇒ 0

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (delete pred? xs)
  (if (null? xs)
      xs
      (append
       (if (pred? (car xs))
           '()
           (list (car xs)))
       (delete pred? (cdr xs)))))

(delete even? '(0 1 2 3)); ⇒ (1 3)
(delete even? '(0 2 4 6)); ⇒ ()
(delete even? '(1 3 5 7)); ⇒ (1 3 5 7)
(delete even? '()); ⇒ ()

"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define (iterate f x n)
  (if (= n 0)
      '()
      (cons x (iterate f (f x) (- n 1)))))

(iterate (lambda (x) (* 2 x)) 1 6); ⇒ (1 2 4 8 16 32)
(iterate (lambda (x) (* 2 x)) 1 1); ⇒ (1)
(iterate (lambda (x) (* 2 x)) 1 0); ⇒ ()

"<<==>><<==>><<==>><<==>><<==>>(((4)))<<==>><<==>><<==>><<==>><<==>>"
(define (intersperse e xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (append (list (car xs) e)
              (intersperse e (cdr xs)))))

(intersperse 'x '(1 2 3 4)); ⇒ (1 x 2 x 3 x 4)
(intersperse 'x '(1 2));     ⇒ (1 x 2)
(intersperse 'x '(1));       ⇒ (1)
(intersperse 'x '());        ⇒ ()

"<<==>><<==>><<==>><<==>><<==>>(((5)))<<==>><<==>><<==>><<==>><<==>>"
(define (any? pred? xs)
  (and (not (null? xs))
       (or (pred? (car xs))
           (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (or (null? xs)
      (and (pred? (car xs))
           (all? pred? (cdr xs)))))

(any? odd? '(1 3 5 7)); ⇒ #t
(any? odd? '(0 1 2 3)); ⇒ #t
(any? odd? '(0 2 4 6)); ⇒ #f
(any? odd? '()); ⇒ #f
""
(all? odd? '(1 3 5 7)); ⇒ #t
(all? odd? '(0 1 2 3)); ⇒ #f
(all? odd? '(0 2 4 6)); ⇒ #f
(all? odd? '()); ⇒ #t ; Это - особенность, реализуйте её

"<<==>><<==>><<==>><<==>><<==>>(((6)))<<==>><<==>><<==>><<==>><<==>>"
(define (o . fs)
  (lambda (x)
    (if (null? fs)
        x
        ((car fs) ((lambda (x) ((apply o (cdr fs)) x)) x)))))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))
    
((o f g h) 1); ⇒ -1
((o f g) 1);   ⇒ 5
((o h) 1);     ⇒ -1
((o) 1);       ⇒ 1