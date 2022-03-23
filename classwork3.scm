"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(load "trace.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss)))))) ; ... и здесь

(zip '(1 2 3) '(one two three))#|
(car xss) => (1 2 3)
xss => ((1 2 3) (one two three))
(car xss) => (2 3)
xss => ((2 3) (two three))
(car xss) => (3)
xss => ((3) (three))
(car xss) => ()|#
;((1 one) (2 two) (3 three))

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
; Пример процедуры с ошибкой
; 
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

; Загружаем каркас
;
(load "unit-test.scm")

; Определяем список тестов
;
(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)
        (test (+ 1 2) #\3)))

; Выполняем тесты
;
(run-tests the-tests)#|
(signum -2) ok
(signum 0) FAIL
  Expected: 0
  Returned: 1
(signum 2) ok|#
;#f

"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax ref
  (syntax-rules ()
    ((ref arr i)
     (letrec ((loop (lambda (ar j)
                      (and (not (null? ar)) (if (= j 0)
                                                (car ar)
                                                (loop (cdr ar) (- j 1)))))))
       (cond ((list? arr) (loop arr i))
             ((vector? arr) (loop (vector->list arr) i))
             ((string? arr) (loop (string->list arr) i))
             (else #f))))
    
    ((ref arr i el)
     (letrec ((loop (lambda (ar j e)
                      (if (= j 0)
                          (cons e ar)
                          (and (not (null? ar))
                               (let ((next (loop (cdr ar) (- j 1) e)))
                                 (and next (cons (car ar) next)))))))
              (check (lambda (type ar)
                       (and ar (type ar)))))
       (cond ((list? arr) (loop arr i el))
             ((vector? arr) (check list->vector (loop (vector->list arr) i el)))
             ((string? arr) (and
                             (char? el)
                             (check list->string (loop (string->list arr) i el))))
             (else #f))))))

(ref '(1 2 3) 1); ⇒ 2
(ref #(1 2 3) 1); ⇒ 2
(ref "123" 1);    ⇒ #\2
(ref "123" 3);    ⇒ #f
""
(ref '(1 2 3) 1 0);   ⇒ (1 0 2 3)
(ref #(1 2 3) 1 0);   ⇒ #(1 0 2 3)
(ref #(1 2 3) 1 #\0); ⇒ #(1 #\0 2 3)
(ref "123" 1 #\0);    ⇒ "1023"
(ref "123" 1 0);      ⇒ #f
(ref "123" 3 #\4);    ⇒ "1234"
(ref "123" 5 #\4);    ⇒ #f
(ref 100500 3 4);     ⇒ #f

"<<==>><<==>><<==>><<==>><<==>>(((4)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax factorize
  (syntax-rules (expt)
    ((factorize '(- (expt a 2) (expt b 2))) '(* (- a b) (+ a b)))
    ((factorize '(- (expt a 3) (expt b 3))) '(* (- a b) (+ (expt a 2) (* a b) (expt b 2))))
    ((factorize '(+ (expt a 3) (expt b 3))) '(* (+ a b) (+ (expt a 2) (- (* a b)) (expt b 2))))))

(factorize '(- (expt x 2) (expt y 2)));  ⇒ (* (- x y) (+ x y))
(factorize '(- (expt x 3) (expt y 3)));  ⇒ (* (- x y) (+ (expt x 2) (* x y) (expt y 2)))
(factorize '(+ (expt x 3) (expt y 3)));  ⇒ (* (+ x y) (+ (expt x 2) (* x y) (expt y 2)))

(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
;  ⇒ (* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1)))

(eval (list (list 'lambda 
                  '(x y) 
                  (factorize '(- (expt x 2) (expt y 2))))
            1 2)
      (interaction-environment))
;  ⇒ -3