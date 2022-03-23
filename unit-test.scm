(define-syntax test
  (syntax-rules ()
    ((test expr expс) `(expr ,expс))))

(define (run-test test)
  (let* ((expr (car test))
         (expc (cadr test))
         (rtrn (eval expr
                     (interaction-environment)))
         (equal (equal? expc rtrn)))
    (begin
      (display expr)
      (if equal
          (display " ok\n")
          (begin
            (display " FAIL\n  Expected: ")
            (write expc)
            (display "\n  Returned: ")
            (write rtrn)
            (newline)))
      equal)))

(define (run-tests tests)
  (letrec ((loop (lambda (x) (or (null? x) (and (car x) (loop (cdr x)))))))
    (loop (map run-test tests))))
