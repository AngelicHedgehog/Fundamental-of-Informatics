(define (interpret program stack)
  (let ((defs '()))
    (let loop ((prog (vector->list program))
               (stck stack))
      ;(write prog)(display "\t")(write stck)(newline)
      (if (null? prog)
          stck
          (let ((now (car prog)) (nxt (cdr prog)))
            (cond ((equal? now '+)
                   (loop nxt
                         (cons (+ (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '-)
                   (loop nxt
                         (cons (- (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '*)
                   (loop nxt
                         (cons (* (cadr stck) (car stck))
                               (cddr stck))))
                  ((equal? now '/)
                   (loop
                    nxt (cons (quotient (cadr stck) (car stck))
                              (cddr stck))))
                  ((equal? now 'mod)
                   (loop
                    nxt (cons (modulo (cadr stck) (car stck))
                              (cddr stck))))
                  ((equal? now 'neg)
                   (loop
                    nxt (cons (- (car stck))
                              (cdr stck))))

                  ((equal? now '=)
                   (loop
                    nxt (cons (if (= (cadr stck) (car stck)) -1 0)
                              (cddr stck))))
                  ((equal? now '>)
                   (loop
                    nxt (cons (if (> (cadr stck) (car stck)) -1 0)
                              (cddr stck))))
                  ((equal? now '<)
                   (loop
                    nxt (cons (if (< (cadr stck) (car stck)) -1 0)
                              (cddr stck))))

                  ((equal? now 'not)
                   (loop
                    nxt (cons (if (= (car stck) 0) -1 0)
                              (cdr stck))))
                  ((equal? now 'and)
                   (loop
                    nxt (cons (if (or (= (cadr stck) 0)
                                      (= (car stck) 0)) 0 -1)
                              (cddr stck))))
                  ((equal? now 'or)
                   (loop
                    nxt (cons (if (and (= (cadr stck) 0)
                                       (= (car stck) 0)) 0 -1)
                              (cddr stck))))

                  ((equal? now 'drop)
                   (loop nxt (cdr stck)))
                  ((equal? now 'swap)
                   (loop
                    nxt (append (list (cadr stck) (car stck))
                                (cddr stck))))
                  ((equal? now 'dup)
                   (loop
                    nxt (append (list (car stck) (car stck))
                                (cdr stck))))
                  ((equal? now 'over)
                   (loop nxt (cons (cadr stck) stck)))
                  ((equal? now 'rot)
                   (loop
                    nxt (append (list (caddr stck) (cadr stck) (car stck))
                                (cdddr stck))))
                  ((equal? now 'depth)
                   (loop nxt (cons (length stck) stck)))

                  ((equal? now 'define)
                   (let loop2 ((body '())
                               (prg (cdr nxt)))
                     (if (equal? (car prg) 'end)
                         (begin
                           (set! defs (cons (cons (car nxt) (reverse body)) defs))
                           (loop (cdr prg) stck))
                         (loop2 (cons (car prg) body) (cdr prg)))))
                  ((equal? now 'exit) stck)
                  ((equal? now 'if)
                   (if (= (car stck) 0)
                       (loop (member 'endif nxt) (cdr stck))
                       (loop nxt (cdr stck))))
                  ((equal? now 'endif) (loop nxt stck))
                  (else (if (integer? now)
                            (loop nxt (cons now stck))
                            (loop nxt (loop (cdr (assoc now defs)) stck))))
                  ))))))