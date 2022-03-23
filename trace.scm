(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex ex) (let ((ax ex))
                     (begin
                       (for-each display `(ex " => " ,ax #\newline))
                       ax)))))