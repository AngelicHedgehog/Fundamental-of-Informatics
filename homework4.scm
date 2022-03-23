;<<==>><<==>><<==>><<==>><<==>>(((A1)))<<==>><<==>><<==>><<==>><<==>>
#|
(((call-with-current-continuation
   (lambda (c) c))
  (lambda (x) x))
 'hello)

При вызове с агрументом продолжние подставляет вместо себя результат
выполения заложенной в себя функции с аргуменом, переданным
продолжению. В данном случае в продолжение от фукнции (lambda (c) c)
передаётся функция (lambda (x) x). Поскольку первая возвращает
переданный ей аргумент без изменений, промежуточным результатом
вычислений будет ((lambda (x) x) 'hello). Далее заметим, что функция
(lambda (x) x) так же возвращает переданный ей аргумент без
изменений, следовательно можно однозначно сказать, что результатом
работы программы обоснованно будет символ 'hello|#
;<<==>><<==>><<==>><<==>><<==>>(((A2)))<<==>><<==>><<==>><<==>><<==>>
(define-syntax my-let
  (syntax-rules ()
    ((_ () do) do)
    ((_ ((var val) . vars) do) (my-let vars ((lambda (var) do) val)))))

(define-syntax my-let*
  (syntax-rules ()
    ((_ () do) do)
    ((_ ((var val) . vars) do) ((lambda (var) (my-let* vars do)) val))))

;<<==>><<==>><<==>><<==>><<==>>(((A3)))<<==>><<==>><<==>><<==>><<==>>
#|В первом примере продолжение "запомнило" позицию вычислений, в
которой значение переменной <x> было равно 100-ам. Поэтому, вызовы
продолжения <r> после вызова функции <f> фактически можно выразить
следующим образом:

(define (r ret)
  (set! x (+ 100
             ret))
  x)

Во втором же примере запрос значения переменной <x> идёт после
объявления продожения, поэтому call/cc с каждым новым вызовом
заново запрашивает значение <x>, которое теперь уже изменяется.
Стало быть вызов продолжения <r> после вызова функции <g>
эквивалентен следующему коду:

(define x 100)
(define (r ret)
  (set! x (+ ret
            x))
  x)

|#

























;<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>
(define memoized-factorial
  (let ((known-results '((0 1))))
    (lambda (n)
      (let* ((arg n)
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (* arg (memoized-factorial (- arg 1)))))
              (set! known-results (cons (list arg res) known-results))
              res))))))

;<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
#|В данном случае макрос предпочтиетльнее процедуры по причине
необходимости сохранения предполагаемо переданного функции вторым
аргументом выражения|#

(define lazy-car car)

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

(define (factorials)
  (let next ((n 0)
             (n! 1))
    (let ((n+1 (+ n 1)))
      (lazy-cons n! (next n+1 (* n! n+1))))))

(define (lazy-factorial n)
  (lazy-ref (factorials) n))

;<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>
(define (read-words)
  (let ((rez
         (let loop ()
           (let* ((char (read-char))
                  (next (if (eof-object? char)
                            '("")
                            (loop))))
             (if (or (eof-object? char) (char-whitespace? char))
                 (if (eq? (car next) "")
                     next
                     (cons "" next))
                 (cons
                  (string-append (string char) (car next))
                  (cdr next)))))))
    (if (and (not (null? rez))
             (eq? (car rez) ""))
        (cdr rez)
        rez)))

;<<==>><<==>><<==>><<==>><<==>>(((4)))<<==>><<==>><<==>><<==>><<==>>
(define-syntax define-struct
  (syntax-rules ()
    ((_ name params)
     (begin
       (let* ((name_ (symbol->string 'name))
              (nw_name (lambda (a) (string->symbol (apply string-append a))))
              (proc (lambda (f . a)
                      (eval `(define ,(nw_name a) ,f)
                            (interaction-environment)))))
         (proc (lambda args
                 (cons 'name
                       (let zip ((a 'params)
                                 (b args))
                         (if (null? a)
                             '()
                             (cons (cons (car a) (car b))
                                   (zip (cdr a) (cdr b))))))) "make-" name_)
         (proc (lambda (obj)
                 (eq? (and (pair? obj) (not (null? obj)) (car obj)) 'name))
               name_ "?")
         (let loop ((prms 'params))
           (or (null? prms)
               (begin
                 (let* ((prm (car prms))
                        (prm_ (symbol->string prm)))
                   (proc (lambda (obj)
                           (cdr (assoc prm (cdr obj))))
                         name_ "-" prm_)
                   (eval `(define-syntax ,(nw_name (list "set-" name_ "-" prm_ "!"))
                            (syntax-rules ()
                              ((_ obj val)
                               (set! obj (cons (car obj)
                                               (let loop ((nw_prms (cdr obj)))
                                                 (if (null? nw_prms)
                                                     '()
                                                     (cons (if (eq? (caar nw_prms) ',prm)
                                                               (cons ',prm val)
                                                               (car nw_prms))
                                                           (loop (cdr nw_prms))))))))))
                         (interaction-environment)))
                 (loop (cdr prms)))))
         )(define void-return #t)))))

;<<==>><<==>><<==>><<==>><<==>>(((5)))<<==>><<==>><<==>><<==>><<==>>
(define-syntax define-data
  (syntax-rules ()
    ((_ type args)
     (begin
       (let loop ((next 'args))
         (or (null? next)
             (begin
               (eval `(define ,(car next)
                        (list 'type ',(car next) ,@(cdar next)))
                     (interaction-environment))
               (eval `(define (,(string->symbol (string-append (symbol->string 'type) "?")) obj)
                        (eq? (and (pair? obj) (not (null? obj)) (car obj)) 'type))
                     (interaction-environment))
               (loop (cdr next)))))(define void-return #t)))))

(define-syntax match
  (syntax-rules ()
    ((_ f ((type . vars) do) . other)
     (if (equal? (caadr f) 'type)
         (just-do-it vars (cddr f) do)
         (match f . other)))
    ((_ . x) #f)))

(define-syntax just-do-it
  (syntax-rules ()
    ((_ () vals do) do)
    ((_ (var . vars) vals do)
     (just-do-it vars (cdr vals)
                  ((lambda (var) do) (car vals))))))