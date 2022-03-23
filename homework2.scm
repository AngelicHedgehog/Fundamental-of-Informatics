"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define (my-range a b d)
  (if (< a b)
      (cons a (my-range (+ a d) b d))
      '()))

(define (my-flatten n); (!!!)
  (letrec ((loop
            (lambda (a b)
              (if (null? a)
                  b
                  (loop (cdr a) (if (list? (car a))
                                    (loop (car a) b)
                                    (cons (car a) b)))))))
    (reverse (loop n '()))))

(define (my-element? x xs)
  (and (not (null? xs))
       (or (eqv? x (car xs))
           (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (if (null? xs)
      xs
      (append
       (if (pred? (car xs))
           (list (car xs))
           '())
       (my-filter pred? (cdr xs)))))

(define (my-fold-left op xs)
  (if (null? (cdr xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

(define (my-fold-right op xs)
  (if (null? (cdr xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(my-range 0 11 3); ⇒ (0 3 6 9)
""
(my-flatten '((1) 2 (3 (4 5)) 6)); ⇒ (1 2 3 4 5 6)
""
(my-element? 1 '(3 2 1)); ⇒ #t
(my-element? 4 '(3 2 1)); ⇒ #f
""
(my-filter odd? (my-range 0 10 1))
;  ⇒ (1 3 5 7 9)
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
;  ⇒ (0 3 6 9 12)
""
(my-fold-left  quotient '(16 2 2 2 2)); ⇒ 1
(my-fold-left  quotient '(1));          ⇒ 1
(my-fold-right expt     '(2 3 4));      ⇒ 2417851639229258349412352
(my-fold-right expt     '(2));          ⇒ 2

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (list->set xs)
  (letrec ((in (lambda (el list)
                 (and (not (null? list)) (or (eqv? el (car list)) (in el (cdr list))))))
           (loop (lambda (x)
                   (if (null? x)
                       '()
                       (let ((nxt (loop (cdr x))))
                         (if (in (car x) nxt)
                             nxt
                             (cons (car x) nxt)))))))
    (loop xs)))

(define (set? xs)
  (letrec ((not-in (lambda (el list)
                     (or (null? list) (and (not (eqv? el (car list))) (not-in el (cdr list))))))
           (loop (lambda (x)
                   (or (null? x) (and (not-in (car x) (cdr x)) (loop (cdr x)))))))
    (loop xs)))

(define (union xs ys)
  (letrec ((not-in (lambda (el list)
                     (or (null? list) (and (not (eqv? el (car list))) (not-in el (cdr list))))))
           (loop (lambda (a b)
                   (cond ((null? b) a)
                         ((not-in (car b) a) (cons (car b) (loop a (cdr b))))
                         (else (loop a (cdr b)))))))
    (loop xs ys)))

(define (intersection xs ys)
  (letrec ((in (lambda (el list)
                 (and (not (null? list)) (or (eqv? el (car list)) (in el (cdr list))))))
           (loop (lambda (a b)
                   (cond ((null? b) '())
                         ((in (car b) a) (cons (car b) (loop a (cdr b))))
                         (else (loop a (cdr b)))))))
    (loop xs ys)))

(define (difference xs ys)
  (letrec ((not-in (lambda (el list)
                     (or (null? list) (and (not (eqv? el (car list))) (not-in el (cdr list))))))
           (loop (lambda (a b)
                   (cond ((null? a) '())
                         ((not-in (car a) b) (cons (car a) (loop (cdr a) b)))
                         (else (loop (cdr a) b))))))
    (loop xs ys)))

(define (symmetric-difference xs ys)
  (difference (append xs ys) (intersection xs ys)))

(define (set-eq? xs ys)
  (and (= (length xs) (length ys))
       (letrec ((in (lambda (el list)
                      (and (not (null? list)) (or (eqv? el (car list)) (in el (cdr list))))))
                (loop (lambda (a b)
                        (or (null? a) (and (in (car a) b) (loop (cdr a) b))))))
         (loop xs ys))))
           

(list->set '(1 1 2 3));                       ⇒ (3 2 1)
(set? '(1 2 3));                              ⇒ #t
(set? '(1 2 3 3));                            ⇒ #f
(set? '());                                   ⇒ #t
(union '(1 2 3) '(2 3 4));                    ⇒ (4 3 2 1)
(intersection '(1 2 3) '(2 3 4));             ⇒ (2 3)
(difference '(1 2 3 4 5) '(2 3));             ⇒ (1 4 5)
(symmetric-difference '(1 2 3 4) '(3 4 5 6)); ⇒ (6 5 2 1)
(set-eq? '(1 2 3) '(3 2 1));                  ⇒ #t
(set-eq? '(1 2) '(1 3));                      ⇒ #f

"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define (string-trim-left s)
  (letrec ((loop (lambda (str flag)
                   (cond ((null? str) '())
                         ((and flag (char-whitespace? (car str))) (loop (cdr str) flag))
                         (flag (cons (car str) (loop (cdr str) (not flag))))
                         (else (cons (car str) (loop (cdr str) flag)))))))
    (list->string (loop (string->list s) #t))))          

(define (string-trim-right s); (!!!)
  (letrec ((loop (lambda (str cache)
                   (cond ((null? str) '())
                         ((char-whitespace? (car str)) (loop (cdr str) (append cache (list (car str)))))
                         (else (append cache (list (car str)) (loop (cdr str) '())))))))
    (list->string (loop (string->list s) '()))))

(define (string-trim s)
  (string-trim-right (string-trim-left s)))

(define (string-prefix? a b)
  (letrec ((loop (lambda (x y)
                   (or (null? x)
                       (and (not (null? y))
                            (and (char=? (car x) (car y))
                                 (loop (cdr x) (cdr y))))))))
    (loop (string->list a) (string->list b))))

(define (string-suffix? a b)
  (letrec ((loop (lambda (x y)
                   (or (equal? x y)
                       (and (not (null? y))
                            (loop x (cdr y)))))))
    (loop (string->list a) (string->list b))))

(define (string-infix? a b)
  (letrec ((loop (lambda (x y)
                   (or (string-prefix? (list->string x) (list->string y))
                       (and (not (null? y))
                            (loop x (cdr y)))))))
    (loop (string->list a) (string->list b))))

(define (string-split str sep)
  (letrec ((seperate (string->list sep))
           (loop (lambda (str! sep! cache-str cache-sep)
                   (cond ((null? sep!) (cons (list->string cache-str) (loop str! seperate '() '())))
                         ((null? str!) (list (list->string (append cache-str cache-sep))))
                         ((char=? (car str!) (car sep!)) (loop (cdr str!) (cdr sep!) cache-str (append cache-sep (list (car str!)))))
                         (else (loop (cdr str!) seperate (append (list (car str!)) cache-str cache-sep) '()))))))
    (loop (string->list str) seperate '() '())))

(string-trim-left  "\t\tabc def");   ⇒ "abc def"
(string-trim-right "abc def\t");     ⇒ "abc def"
(string-trim       "\t abc def \n"); ⇒ "abc def"
""
(string-prefix? "abc" "abcdef");  ⇒ #t
(string-prefix? "bcd" "abcdef");  ⇒ #f
(string-prefix? "abcdef" "abc");  ⇒ #f
""
(string-suffix? "def" "abcdef");  ⇒ #t
(string-suffix? "bcd" "abcdef");  ⇒ #f
""
(string-infix? "def" "abcdefgh"); ⇒ #t
(string-infix? "abc" "abcdefgh"); ⇒ #t
(string-infix? "fgh" "abcdefgh"); ⇒ #t
(string-infix? "ijk" "abcdefgh"); ⇒ #f
(string-infix? "bcd" "abc");      ⇒ #f
""
(string-split "x;y;z" ";");       ⇒ ("x" "y" "z")
(string-split "x-->y-->z" "-->"); ⇒ ("x" "y" "z")

"<<==>><<==>><<==>><<==>><<==>>(((4)))<<==>><<==>><<==>><<==>><<==>>"
(define (make-multi-vector sizes)
  (letrec ((mult (lambda (a b)
                   (if (null? b)
                       a
                       (mult (* a (car b)) (cdr b))))))
    (list (make-vector (mult 1 sizes)) sizes)))

(define (make-multi-vector sizes fill)
  (letrec ((mult (lambda (a b)
                   (if (null? b)
                       a
                       (mult (* a (car b)) (cdr b))))))
    (list (make-vector (mult 1 sizes) fill) sizes)))

(define (multi-vector? m)
  (letrec ((mult (lambda (a b)
                   (if (null? b)
                       a
                       (mult (* a (car b)) (cdr b))))))
    (and (list? m)
         (= (length m) 2)
         (vector? (car m))
         (list? (cadr m))
         (= (vector-length (car m)) (mult 1 (cadr m))))))

(define (multi-vector-ref m indices)
  (letrec ((mult (lambda (a b)
                   (if (null? b)
                       a
                       (mult (* a (car b)) (cdr b)))))
           (index (lambda (ind1 ind2)
                    (if (null? ind1)
                        0
                        (+ (* (car ind1) (mult 1 (cdr ind2))) (index (cdr ind1) (cdr ind2)))))))
    (vector-ref (car m) (index indices (cadr m)))))

(define (multi-vector-set! m indices x)
  (letrec ((mult (lambda (a b)
                   (if (null? b)
                       a
                       (mult (* a (car b)) (cdr b)))))
           (index (lambda (ind1 ind2)
                    (if (null? ind1)
                        0
                        (+ (* (car ind1) (mult 1 (cdr ind2))) (index (cdr ind1) (cdr ind2)))))))
    (vector-set! (car m) (index indices (cadr m)) x)))

(define m (make-multi-vector '(11 12 9 16) 1))
(multi-vector? m); ⇒ #t
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12)); ⇒ test

; Индексы '(1 2 1 1) и '(2 1 1 1) — разные индексы
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)); ⇒ X
(multi-vector-ref m '(2 1 1 1)); ⇒ Y

(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0)); ⇒ -1

"<<==>><<==>><<==>><<==>><<==>>(((5)))<<==>><<==>><<==>><<==>><<==>>"
(define (o . fs); (!!!)
  (lambda (x)
    (my-fold-right
     (lambda (f a) (f a))
     (append fs (list x)))))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

((o f g h) 1); ⇒ -1
((o f g) 1);   ⇒ 5
((o h) 1);     ⇒ -1
((o) 1);       ⇒ 1

"<<==>><<==>><<==>><<==>><<==>>(((А)))<<==>><<==>><<==>><<==>><<==>>"
(define (my-flatten n); (!!!)
  (letrec ((loop
            (lambda (list cache)
              (if (null? list)
                  (reverse cache)
                  (loop (if (or
                             (equal? (car list) '())
                             (not (list? (car list))))
                            (cdr list)
                            (cons (caar list) (cons (cdar list) (cdr list))))
                        (if (list? (car list))
                            cache
                            (cons (car list) cache)))))))
    (loop n '())))

(my-flatten '((1) 2 (3 (4 5)) 6)); ⇒ (1 2 3 4 5 6)
""

(define (list-trim-right s); (!!!)
  (if (null? s)
      '()
      (let ((nxt (list-trim-right (cdr s))))
        (if (and (char-whitespace? (car s)) (null? nxt))
            '()
            (cons (car s) nxt)))))

(list-trim-right '(#\a #\b #\c #\space #\d #\e #\f #\tab));     ⇒ (#\a #\b #\c #\space #\d #\e #\f)
""

(define (o . fs); (!!!)
  (lambda (x)
    (my-fold-right
     (lambda (f a) (f a))
     (append fs (list x)))))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

((o f g h) 1); ⇒ -1
((o f g) 1);   ⇒ 5
((o h) 1);     ⇒ -1
((o) 1);       ⇒ 1
