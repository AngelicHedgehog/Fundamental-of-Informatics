"<<==>><<==>><<==>><<==>><<==>>(((1)))<<==>><<==>><<==>><<==>><<==>>"
(define (check-frac fract)
  (and (string? fract)
       (not (string=? fract ""))
       (let* ((frac (string->list fract))
              (first (car frac))
              (chisl #f)
              (drob #f)
              (znam #f))
         (and
          (not (null? (cdr frac)))
          (or (char=? first #\+)
              (char=? first #\-)
              (char-numeric? first))
          (set! frac (cdr frac)))
         (let loop ((a (car frac))
                    (b (cdr frac)))
           (and
            (cond ((char-numeric? a)
                   (if drob
                       (set! znam #t)
                       (set! chisl #t)))
                  ((and (char=? a #\/) (eq? drob #f))
                   (set! drob #t))
                  (else #f))
            (if (null? b)
                (and chisl drob znam)
                (loop (car b) (cdr b))))))))

(check-frac "110/111") ;⇒ #t
(check-frac "-4/3")    ;⇒ #t
(check-frac "+5/10")   ;⇒ #t
(check-frac "5.0/10")  ;⇒ #f
(check-frac "FF/10")   ;⇒ #f


(define (scan-frac fract)
  (and (string? fract)
       (not (string=? fract ""))
       (let* ((frac (string->list fract))
              (first (car frac))
              (sign 1)
              (chisl 0)
              (drob #f)
              (znam 0))
         (and
          (not (null? (cdr frac)))
          (cond ((char=? first #\+) (set! frac (cdr frac)))
                ((char=? first #\-) (begin
                                      (set! frac (cdr frac))
                                      (set! sign -1))))
          (let loop ((a (car frac))
                     (b (cdr frac)))
            (and
             (cond ((char-numeric? a)
                    (let ((int (- (char->integer a) (char->integer #\0))))
                      (if drob
                          (set! znam (+ (* znam 10) int))
                          (set! chisl (+ (* chisl 10) int)))))
                   ((and (char=? a #\/) (eq? drob #f))
                    (set! drob #t))
                   (else #f))
             (if (null? b)
                 (and drob (not (= znam 0)) (/ (* sign chisl) znam))
                 (loop (car b) (cdr b)))))))))

(scan-frac "110/111")  ;⇒ 110/111
(scan-frac "-4/3")     ;⇒ -4/3
(scan-frac "+5/10")    ;⇒ 1/2
(scan-frac "5.0/10")   ;⇒ #f
(scan-frac "FF/10")    ;⇒ #f


(define (scan-many-fracs fracts)
  (let ((rez '(()))
        (fracs (string->list fracts)))
    (let loop ((a (car fracs))
               (b (cdr fracs)))
      (if (char-whitespace? a)
          (or (null? (car rez)) (set! rez (cons '() rez)))
          (set! rez (cons (cons a (car rez)) (cdr rez))))
      (or (null? b) (loop (car b) (cdr b))))
    (let ((scans
           (let loop ((frac rez))
      (if (null? frac)
          '()
          (let ((scan (scan-frac (list->string (reverse (car frac))))))
            (and scan (cons scan (loop (cdr frac)))))))))
      (and scans (reverse scans))
    )))

(scan-many-fracs
 "\t1/2 1/3\n\n10/8")  ;⇒ (1/2 1/3 5/4)
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")  ;⇒ #f

"<<==>><<==>><<==>><<==>><<==>>(((2)))<<==>><<==>><<==>><<==>><<==>>"
(define (parse prog)
  (let loop ((articles_ '())
             (body_ (vector->list prog)))
    (if (not (member 'end body_))
        (let ((_articles_ (articles (reverse articles_)))
              (_body_ (body body_)))
          (and _articles_ _body_ (list _articles_ _body_)))
        (loop (cons (car body_) articles_) (cdr body_)))))

(define (articles prog)
  (if (null? prog)
      '()
      (let loop ((article_ '())
                 (articles_ prog))
        (cond ((equal? (car articles_) 'end)
               (let ((_article_ (article (reverse (cons 'end article_))))
                     (_articles_ (articles (cdr articles_))))
                 (and _article_ _articles_ (cons _article_ _articles_))))
              ((null? articles_) #f)
              (else
               (loop (cons (car articles_) article_) (cdr articles_)))))))

(define (article article_)
  (and (> (length article_) 2)
       (equal? (car article_) 'define)
       (not (member (cadr article_) '(define end if endif)))
       (member 'end article_)
       (null? (cdr (member 'end article_)))
       (let loop ((body_ '())
                  (article__ (cddr article_)))
         (if (equal? article__ '(end))
             (list (cadr article_) (body (reverse body_)))
             (and (not (equal? (car article__) 'define))
                  (loop (cons (car article__) body_) (cdr article__)))))))


(define (body body_)
  (cond ((null? body_) '())
        ((member (car body_) '(define endif)) #f)
        ((equal? (car body_) 'if)
         (and (member 'endif body_)
              (let loop ((body__ '())
                         (prog_ (cdr body_))
                         (depth 1))
                (cond ((= depth 0)
                       (let ((body1 (body (reverse (cdr body__))))
                             (body2 (body prog_)))
                         (and body1 body2 (cons (list 'if body1) body2))))
                      ((or (null? prog_) (< depth 0)) #f)
                      (else
                       (loop (cons (car prog_) body__)
                             (cdr prog_)
                             (+ depth (cond ((equal? (car prog_) 'if) 1)
                                            ((equal? (car prog_) 'endif) -1)
                                            (else 0))))))
                )))
        (else (let ((_body_ (body (cdr body_))))
                (and _body_ (cons (car body_) _body_))))))


(parse #(1 2 +)) ;⇒ (() (1 2 +))

(parse #(x dup 0 swap if drop -1 endif))
    ;⇒ (() (x dup 0 swap (if (drop -1))))

(parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial ))
 #|⇒
 (((-- (1 -))
   (=0? (dup 0 =))
   (=1? (dup 1 =))
   (factorial
    (=0? (if (drop 1 exit))
     =1? (if (drop 1 exit))
     dup --
     factorial
     *)))
  (0 factorial
   1 factorial
   2 factorial
   3 factorial
   4 factorial))|#

(parse #(define word w1 w2 w3)) ;⇒ #f

(parse #(   define =0? dup 0 = end
                define <0? dup 0 < end
                define signum
                    =0? if exit endif
                    <0? if drop -1 exit endif
                    drop
                    1
                end
                 0 signum
                -5 signum
                10 signum       ))
 #|⇒
(((=0? (dup 0 =))
  (<0? (dup 0 <))
  (signum
   (=0? (if (exit))
    <0? (if (drop -1 exit))
    drop
    1)))
 (0 signum
  -5 signum
  10 signum))|#

(parse #(   define =0? dup 0 = end
                define =1? dup 1 = end
                define -- 1 - end
                define fib
                    =0? if drop 0 exit endif
                    =1? if drop 1 exit endif
                    -- dup
                    -- fib
                    swap fib
                    +
                end
                define make-fib
                    dup 0 < if drop exit endif
                    dup fib
                    swap --
                    make-fib
                end
                10 make-fib     ))
 #|⇒
(((=0? (dup 0 =))
  (=1? (dup 1 =))
  (-- (1 -))
  (fib
   (=0? (if (drop 0 exit))
    =1? (if (drop 1 exit))
    -- dup
    -- fib
    swap fib
    +))
  (make-fib
   (dup 0 < (if (drop exit))
    dup fib
    swap --
    make-fib)))
 (10 make-fib))|#

(parse #(   define =0? dup 0 = end
                define gcd
                    =0? if drop exit endif
                    swap over mod
                    gcd
                end
                90 99 gcd
                234 8100 gcd    ))
 #|⇒
(((=0? (dup 0 =))
  (gcd
   (=0? (if (drop exit))
    swap over mod
    gcd)))
 (90 99 gcd
  234 8100 gcd))|#