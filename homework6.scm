(define var_chars
  (string->list "abcdefghijklmnopqrstuvwxyz"))

(define int_chars
  (string->list "0123456789.e+"))

(define operators
  (let ((a (string->list "+-*/^")))
    `(,@a ,@(map string->symbol (map string a)))))

;"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define (tokenize expression)
  (let ((cash-clear
         (lambda (csh int)
           (let ((str (list->string (reverse csh))))
             (if int (string->number str) (string->symbol str))))))
    (let loop ((expr (string->list expression))
               (tokens '())
               (cash '())
               (int? #f))
      (cond ((null? expr) (let ((int-var (cash-clear cash int?)))
                            (and int-var (reverse
                                          (if (null? cash)
                                              tokens
                                              (cons int-var tokens))))))
            ((char-whitespace? (car expr)) (loop (cdr expr) tokens cash int?))
            ((member (car expr) operators)
             (let ((int-var (cash-clear cash int?)))
               (and int-var
                    (loop (cdr expr) (cons (string->symbol (string (car expr)))
                                           (if (null? cash)
                                               tokens
                                               (cons int-var tokens))) '() #f))))
            ((or (char=? (car expr) #\()
                 (char=? (car expr) #\)))
             (let ((int-var (cash-clear cash int?)))
               (and int-var
                    (loop (cdr expr)
                          (cons (string (car expr))
                                (if (null? cash)
                                    tokens
                                    (cons int-var tokens)))
                          '()
                          #f))))
            ((or int? (and (null? cash)
                           (member (car expr) int_chars)
                           (not (char=? (car expr) #\e))))
             (and (member (car expr) int_chars)
                  (if (char=? (car expr) #\e)
                      (let loop2 ((exp (cdr expr)))
                        (cond ((null? exp) #f)
                              ((char-whitespace? (car exp)) (loop2 (cdr exp)))
                              (else (loop (cdr exp)
                                          tokens
                                          (cons (car exp) (cons #\e cash))
                                          #t))))
                      (and (member (car expr) int_chars)
                           (loop (cdr expr) tokens (cons (car expr) cash) #t)))))

            (else (and (member (car expr) var_chars)
                       (loop (cdr expr) tokens (cons (car expr) cash) #f)))
            )
      )))
#|
(tokenize "1")
;⇒ (1)

(tokenize "-a")
;⇒ (- a)

(tokenize "-a + b * x^2 + dy")
;⇒ (- a + b * x ^ 2 + dy)

(tokenize "(a - 1)/(b + 1)")
;⇒ ("(" a - 1 ")" / "(" b + 1 ")")
|#
;"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (parse tokens)
  (and
   (list? tokens)
   (not (null? tokens))
   (set!
    tokens
    (let loop ((_tokens tokens)
               (rez_tokens '())
               (depth 0)
               (brkts '()))
      (cond ((null? _tokens) (and
                              (= depth 0)
                              (not (member (car rez_tokens) operators))
                              (reverse rez_tokens)))
            ((and (= depth 1) (equal? (car _tokens) ")"))
             (let ((pars (parse (reverse brkts))))
               (and pars
                    (not (null? pars))
                    (loop (cdr _tokens)
                          (let ((lst
                                 (let loop2 ((list pars))
                                   (if (and (list? list)
                                            (null? (cdr list)))
                                       (loop2 (car list))
                                       list))))
                            (cons lst rez_tokens)) 0 '()))))
            ((> depth 0)
             (loop (cdr _tokens)
                   rez_tokens
                   (+ depth
                      (cond ((equal? (car _tokens) "(") 1)
                            ((equal? (car _tokens) ")") -1)
                            (else 0)))
                   (cons (car _tokens) brkts)))
            ((equal? (car _tokens) "(")
             (loop (cdr _tokens) rez_tokens 1 '()))
            ((or (< depth 0) (string? (car _tokens))) #f)
            (else (loop (cdr _tokens) (cons (car _tokens) rez_tokens) depth '()))
            )
      )))

  (and
   tokens
   (not (null? tokens))
   (set!
    tokens
    (let loop ((_tokens (reverse tokens))
               (rez_tokens '()))
      (cond ((null? _tokens) rez_tokens)
            ((equal? (car _tokens) '^)
             (and (not (or (null? _tokens)
                           (null? rez_tokens)
                           (member (cadr _tokens) operators)
                           (member (car rez_tokens) operators)))
                  (loop (cddr _tokens)
                        (cons (list (cadr _tokens) '^ (car rez_tokens))
                              (cdr rez_tokens)))))
            ((and (equal? (car _tokens) '-)
                  (or (null? (cdr _tokens))
                      (member (cadr _tokens) operators)))
             (and (not (or
                        (null? rez_tokens)
                        (member (car rez_tokens) operators)))
                  (loop (cdr _tokens)
                        (cons (list '- (car rez_tokens))
                              (cdr rez_tokens)))))
            (else (loop (cdr _tokens) (cons (car _tokens) rez_tokens)))
            )
      )))
  (and
   tokens
   (not (null? tokens))
   (set!
    tokens
    (let loop ((_tokens tokens)
               (rez_tokens '()))
      (cond ((null? _tokens) (reverse rez_tokens))
            ((or
              (equal? (car _tokens) '*)
              (equal? (car _tokens) '/))
             (and (not (or
                        (null? (cdr tokens))
                        (null? rez_tokens)
                        (member (cadr _tokens) operators)
                        (member (car rez_tokens) operators)))
                  (loop (cddr _tokens)
                        (cons (list (car rez_tokens)
                                    (car _tokens)
                                    (cadr _tokens)) (cdr rez_tokens)))))
            (else (loop (cdr _tokens) (cons (car _tokens) rez_tokens)))
            )
      )))

  (and
   tokens
   (not (null? tokens))
   (set!
    tokens
    (let loop ((_tokens tokens)
               (rez_tokens '()))
      (cond ((null? _tokens) (if (null? (cdr rez_tokens))
                                 (car rez_tokens)
                                 (reverse rez_tokens)))
            ((or
              (equal? (car _tokens) '+)
              (equal? (car _tokens) '-))
             (and (not (or
                        (null? (cdr tokens))
                        (null? rez_tokens)
                        (member (cadr _tokens) operators)
                        (member (car rez_tokens) operators)))
                  (loop (cddr _tokens)
                        (cons (list (car rez_tokens)
                                    (car _tokens)
                                    (cadr _tokens)) (cdr rez_tokens)))))
            (else (loop (cdr _tokens) (cons (car _tokens) rez_tokens)))
            )
      )))
  (and
   tokens
   (or
    (and (not (list? tokens)) tokens)
    (and
     (not (null? tokens))
     (let loop ((_tokens tokens)
                (var #t))
       (cond ((null? _tokens) (not var))
             ((member (car _tokens) '(#\( #\))) (loop (cdr _tokens) var))
             ((eq? (car _tokens) '-) (loop (cdr _tokens) #t))
             ((member (car _tokens) operators) (and
                                                (not var)
                                                (loop (cdr _tokens) (not var))))
             (else (and var (loop (cdr _tokens) (not var))))
             )
       )
     tokens)))
  )
#|
; Ассоциативность левая
;
(parse (tokenize "a/b/c/d"))
;⇒ (((a / b) / c) / d)

; Ассоциативность правая
;
(parse (tokenize "a^b^c^d"))
;⇒ (a ^ (b ^ (c ^ d)))

; Порядок вычислений задан скобками
;
(parse (tokenize "a/(b/c)"))
;⇒ (a / (b / c))

; Порядок вычислений определен только
; приоритетом операций
;
(parse (tokenize "a + b/c^2 - d"))
;⇒ ((a + (b / (c ^ 2))) - d)
|#
;"<<==>><<==>><<==>><<==>><<==>>(((3)))<<==>><<==>><<==>><<==>><<==>>"
(define (tree->scheme tree)
  (cond ((not (list? tree)) tree)
        ((equal? (car tree) '-)
         (list '- (tree->scheme (cadr tree))))
        (else (list (if (equal? (cadr tree) '^)
                        'expt
                        (cadr tree))
                    (tree->scheme (car tree))
                    (tree->scheme (caddr tree)
                                  )))
        )
  )
#|
(tree->scheme (parse (tokenize "x^(a + 1)")))
  ;⇒ (expt x (+ a 1))

(eval (tree->scheme (parse (tokenize "2^2^2^2")))
      (interaction-environment))
  ;⇒ 65536
|#