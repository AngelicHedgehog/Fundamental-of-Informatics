'feature-if-else
'feature-nested-if
'feature-while-loop
'feature-repeat-loop
'feature-for-loop
'feature-break-continue
'feature-switch-case
'feature-hi-level
'feature-tail-call
'feature-global

(define (interpret program stack)
  (let ((globals '())); (hi-defs '()))
    (let loop ((prog (vector->list program))
               (locals '())
               (stck stack))
      #|| #
      (write (or (null? prog) (car prog)))(display "\t")
      ;(write prog)(display "\t")
      (write stck)(display "\t")
      ;(write globals)(display "\t")
      ;(write locals)(display "\t")
      (newline)
      # ||#
      (if (null? prog)
          stck
          (let ((now (car prog)) (nxt (cdr prog)))
            (cond ((equal? now '+)
                   (loop nxt locals
                         (cons (+ (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '-)
                   (loop nxt locals
                         (cons (- (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '*)
                   (loop nxt locals
                         (cons (* (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '/)
                   (loop nxt locals
                         (cons (quotient (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now 'mod)
                   (loop nxt locals
                         (cons (modulo (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now 'neg)
                   (loop nxt locals
                         (cons (- (car stck))
                               (cdr stck))))

                  ((equal? now '=)
                   (loop nxt locals
                         (cons (if (= (cadr stck) (car stck)) -1 0)
                               (cddr stck))))
                  ((equal? now '>)
                   (loop nxt locals
                         (cons (if (> (cadr stck) (car stck)) -1 0)
                               (cddr stck))))
                  ((equal? now '<)
                   (loop nxt locals
                         (cons (if (< (cadr stck) (car stck)) -1 0)
                               (cddr stck))))

                  ((equal? now 'not)
                   (loop nxt locals
                         (cons (if (= (car stck) 0) -1 0)
                               (cdr stck))))
                  ((equal? now 'and)
                   (loop nxt locals
                         (cons (if (or (= (cadr stck) 0)
                                       (= (car stck) 0)) 0 -1)
                               (cddr stck))))
                  ((equal? now 'or)
                   (loop nxt locals
                         (cons (if (and (= (cadr stck) 0)
                                        (= (car stck) 0)) 0 -1)
                               (cddr stck))))

                  ((equal? now 'drop)
                   (loop nxt locals (cdr stck)))
                  ((equal? now 'swap)
                   (loop nxt locals
                         (append (list (cadr stck) (car stck))
                                 (cddr stck))))
                  ((equal? now 'dup)
                   (loop nxt locals
                         (append (list (car stck) (car stck))
                                 (cdr stck))))
                  ((equal? now 'over)
                   (loop nxt locals (cons (cadr stck) stck)))
                  ((equal? now 'rot)
                   (loop nxt locals
                         (append (list (caddr stck) (cadr stck) (car stck))
                                 (cdddr stck))))
                  ((equal? now 'depth)
                   (loop nxt locals (cons (length stck) stck)))

                  ((equal? now 'define)
                   (let loop2 ((body '())
                               (prg (cdr nxt)))
                     (if (equal? (car prg) 'end)
                         (begin
                           (set! globals (cons (cons (car nxt) (reverse body)) globals))
                           (loop (cdr prg) locals stck))
                         (loop2 (cons (car prg) body) (cdr prg)))))
                  ((equal? now 'exit) stck)

                  ((equal? now 'if)
                   (let ((branches
                          (let loop2 ((branch '(())) (noww (car nxt)) (nxxt (cdr nxt)) (depth 1))
                            (cond ((and (= depth 1) (equal? noww 'endif))
                                   (cons nxxt branch))
                                  ((and (= depth 1) (equal? noww 'else))
                                   (loop2 (cons '() branch) (car nxxt) (cdr nxxt) depth))
                                  (else
                                   (loop2 (cons (cons noww (car branch)) (cdr branch))
                                          (car nxxt) (cdr nxxt) (+ depth (cond ((equal? noww 'if) 1)
                                                                               ((equal? noww 'endif) -1)
                                                                               (else 0)))))))))
                     (loop (append (reverse
                                    (if (null? (cddr branches))
                                        (if (= (car stck) 0) '() (cadr branches))
                                        (if (= (car stck) 0) (cadr branches) (caddr branches))))
                                   (car branches)) locals (cdr stck))))

                  ((equal? now 'while)
                   (let ((bodyy
                          (let loop2 ((body '()) (noww (car nxt)) (nxxt (cdr nxt)) (depth 1))
                            (if (and (= depth 1) (equal? noww 'wend))
                                (list nxxt (reverse body))
                                (loop2 (cons noww body) (car nxxt) (cdr nxxt)
                                       (+ depth (cond ((equal? noww 'while) 1)
                                                      ((equal? noww 'wend) -1)
                                                      (else 0))))))))
                     (loop (car bodyy) locals
                           (let loop2 ((stc stck))
                             (if (= (car stc) 0)
                                 (cdr stc)
                                 (loop2 (loop (cadr bodyy) locals (cdr stc))))))))

                  ((equal? now 'repeat)
                   (let ((bodyy
                          (let loop2 ((body '()) (noww (car nxt)) (nxxt (cdr nxt)) (depth 1))
                            (if (and (= depth 1) (equal? noww 'until))
                                (list nxxt (reverse body))
                                (loop2 (if (and (equal? noww 'break)
                                                (= depth 1))
                                           (cons 'continue (cons -1 body))
                                           (cons noww body)) (car nxxt) (cdr nxxt)
                                       (+ depth (cond ((equal? noww 'repeat) 1)
                                                      ((equal? noww 'until) -1)
                                                      (else 0))))))))
                     (loop (car bodyy) locals
                           (let loop2 ((stc (loop (cadr bodyy) locals stck)))
                             (if (= (car stc) 0)
                                 (loop2 (loop (cadr bodyy) locals (cdr stc)))
                                 (cdr stc)
                                 )))))

                  ((equal? now 'for)
                   (let ((bodyy
                          (let loop2 ((body '()) (noww (car nxt)) (nxxt (cdr nxt)))
                            (if (equal? noww 'next)
                                (list nxxt (reverse (cons '- (cons 'i (cons (car stck) body)))))
                                (loop2 (if (equal? noww 'continue)
                                           (cons noww (cons '- (cons 'i (cons (car stck) body))))
                                           (cons noww body)) (car nxxt) (cdr nxxt))))))
                     (loop (car bodyy) locals
                           (let loop2 ((stc (cons (- (car stck) (cadr stck) -1) (cddr stck))))
                             (if (= (car stc) 0)
                                 (cdr stc)
                                 (loop2 (loop (cadr bodyy)
                                              (cons (list 'i (- (car stck) (car stc) -1)) locals)
                                              (cdr stc))))))))

                  ((equal? now 'break) (cons 0 stck))
                  ((equal? now 'continue) stck)

                  ((equal? now 'switch)
                   (loop (let loop2 ((body '())
                                     (noww (car nxt))
                                     (nxxt (cdr nxt))
                                     (case? #f))
                           (cond ((and (equal? noww 'case) (or
                                                            (= (car nxxt) (car stck))
                                                            case?))
                                  (loop2 body (cadr nxxt) (cddr nxxt) #t))
                                 ((equal? noww 'exitcase)
                                  (loop2 body (car nxxt) (cdr nxxt) #f))
                                 ((equal? noww 'endswitch)
                                  (append (reverse body) nxxt))
                                 (else
                                  (loop2 (if case?
                                             (cons noww body)
                                             body) (car nxxt) (cdr nxxt) case?))))
                         locals (cdr stck)))

                  ((equal? now '&) 
                   (loop (cdr nxt) locals
                         (cons (cdr (or
                                     (assoc (car nxt) globals)
                                     (assoc (car nxt) locals)))
                               stck)))
                  ((equal? now 'lam)
                   (let loop2 ((body '()) (noww (car nxt)) (nxxt (cdr nxt)) (depth 1))
                     (if (= depth 0)
                         (loop (cons noww nxxt) locals (cons (reverse (cdr body)) stck))
                         (loop2 (cons noww body)
                                (car nxxt)
                                (cdr nxxt)
                                (+ depth
                                   (cond ((equal? noww 'lam) 1)
                                         ((equal? noww 'endlam) -1)
                                         (else 0)))))
                     ))
                  ((equal? now 'apply) (loop nxt locals
                                             (loop (car stck) locals (cdr stck))))

                  ((equal? now 'tail) (loop (list (car nxt)) locals stck))

                  ((equal? now 'defvar)
                   (begin
                     (set! globals
                           (cons (list (car nxt) (cadr nxt)) globals))
                     (loop (cddr nxt) locals stck)))
                  ((equal? now 'set)
                   (begin
                     (set! globals
                           (cons (list (car nxt) (car stck)) globals))
                     (loop (cdr nxt) locals (cdr stck))))

                  (else (if (integer? now)
                            (loop nxt locals (cons now stck))
                            (loop nxt locals (loop (cdr (or
                                                         (assoc now globals)
                                                         (assoc now locals)))
                                                   locals stck))))
                  ))))))