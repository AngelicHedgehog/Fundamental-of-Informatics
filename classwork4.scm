"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define break #f)

(define (use-assertions)
  (call-with-current-continuation (lambda (a)
                                    (set! break a))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr) (or expr (begin
                         (display "FAILED: ")
                         (write 'expr)
                         (break))))))


(use-assertions) ; Инициализация вашего каркаса перед использованием

; Определение процедуры, требующей верификации переданного ей значения:

(define (1/x x)
  (assert (not (zero? x))) ; Утверждение: x ДОЛЖЕН БЫТЬ ≠ 0
  (/ 1 x))

; Применение процедуры с утверждением:

(map 1/x '(1 2 3 4 5)) ; ВЕРНЕТ список значений в программу

(map 1/x '(-2 -1 0 1 2)) ; ВЫВЕДЕТ в консоль сообщение и завершит работу программы

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (save-data data path)
  (call-with-output-file path
    (lambda (file)
      (write data file))))

(define (load-data path)
  (call-with-input-file path
    (lambda (file)
      (read file))))

(define (reader path)
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let ((char (read-char port)))
          (if (eof-object? char)
              '()
              `(, char ,@(loop))))))))

(define (count_lines path)
  (let loop ((file (reader path))
             (n 0)
             (line #f))
    (if (null? file)
        (+ n (if line 1 0))
        (if (eq? (car file) #\newline)
            (loop (cdr file) (+ n (if line 1 0)) #f)
            (loop (cdr file) n
                  (or line (not (eq? (car file) #\return))))))))

(apply * '(1 2 3 4))        ; 24
(save-data '(1 2 3 4) "1234.txt")
(load-data "1234.txt")                      ; (1 2 3 4)
(apply * (load-data "1234.txt"))  ; 24

(save-data '(1 #\2 #(3 4) "5" '6) "data.txt")
(load-data "data.txt")
(count_lines "ЛР4.rkt")

"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define (trib n)
  (cond ((< n 2) 0)
        ((= n 2) 1)
        (else (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3))))))


(define memmory '(((trib-mem 2) . 1)
                  ((trib-mem 1) . 0)
                  ((trib-mem 0) . 0)))

(define (trib-mem n)
  (or (letrec ((loop (lambda (m)
                       (and (not (null? m))
                            (if (equal? `(trib-mem ,n) (caar m))
                                (cdar m)
                                (loop (cdr m)))))))
        (loop memmory))
      (let ((rez (+ (trib-mem (- n 1))
                    (trib-mem (- n 2))
                    (trib-mem (- n 3)))))
        (begin
          (set! memmory (cons `((trib-mem ,n) . ,rez) memmory))
          rez))))

(trib-mem 10)
memmory

"<<==>><<==>><<==>><<==>><<==>>(((4)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax my-if
  (syntax-rules ()
    ((_ condition do_when do_unless)
     (force (or (and condition
                     (delay do_when))
                (delay do_unless))))))


(my-if #t 1 (/ 1 0)); ⇒ 1
(my-if #f (/ 1 0) 1); ⇒ 1

"<<==>><<==>><<==>><<==>><<==>>(((5)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax my-let
  (syntax-rules ()
    ((_ () do) do)
    ((_ ((var val) . vars) do) (my-let vars ((lambda (var) do) val)))))

(define-syntax my-let*
  (syntax-rules ()
    ((_ () do) do)
    ((_ ((var val) . vars) do) ((lambda (var) (my-let* vars do)) val))))


(my-let ((x 2) (y 3))
        (my-let ((x 7)
                 (z (+ x y)))
                (* z x))); 35

(my-let ((x 2) (y 3))
        (my-let* ((x 7)
                  (z (+ x y)))
                 (* z x))); 70

"<<==>><<==>><<==>><<==>><<==>>(((6А)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax when
  (syntax-rules ()
    ((when cond? . exprs) (and cond? (begin . exprs)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? . exprs) (and (not cond?) (begin . exprs)))))


(define x 1)
; Пусть x = 1
;
(when   (> x 0) (display "x > 0") (newline))
(unless (= x 0) (display "x != 0") (newline))

"<<==>><<==>><<==>><<==>><<==>>(((6Б)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . exprs)
     (letrec ((loop (lambda (arr)
                      (and (not (null? arr))
                           (let ((x (car arr)))
                             (begin (begin . exprs)
                                    (loop (cdr arr))))))))
       (loop xs)))
    ((for xs as x . exprs) (for x in xs . exprs))))


(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

"<<==>><<==>><<==>><<==>><<==>>(((6В)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax while
  (syntax-rules ()
    ((while cond? . exprs)
     (letrec ((loop (lambda ()
                      (and cond?
                           (begin (begin . exprs)
                                  (loop))))))
       (loop)))))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

"<<==>><<==>><<==>><<==>><<==>>(((6Г)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax repeat
  (syntax-rules ()
    ((repeat exprs until cond?)
     (letrec ((loop (lambda ()
                      (begin (begin . exprs)
                             (and (not cond?) (loop))))))
       (loop)))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))

"<<==>><<==>><<==>><<==>><<==>>(((6Д)))<<==>><<==>><<==>><<==>><<==>>"
(define-syntax cout
  (syntax-rules ()
    ((cout . cmds)
     (letrec ((loop (lambda (cmds_)
                      (if (null? cmds_)
                          (display "")
                          (begin
                            (let ((cmd (cadr cmds_)))
                              (if (equal? cmd 'endl)
                                  (newline)
                                  (display cmd)))
                            (loop (cddr cmds_)))))))
       (loop 'cmds)))))


(cout << "a = " << 1 << endl << "b = " << 2 << endl)
