(define-syntax interpret
  (syntax-rules (ev
                 + - * / mod neg
                 = > <
                 not and or
                 drop swap dup over rot depth
                 define end exit if endif)
    ((_ prog stc) (let ((defs '())) (interpret '() prog stc)))
    ((_ b ev prog stc) (begin ;(write prog) (display "\t")(write stc) (newline)(newline)
                              (eval `(interpret b ,prog ',stc) (interaction-environment))))
    ((_ b #() stack) stack)

    ((_ b #(+ program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (+ n1 n2) stack ...)))

    ((_ b #(- program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (- n1 n2) stack ...)))

    ((_ b #(* program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (* n1 n2) stack ...)))

    ((_ b #(/ program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (quotient n1 n2) stack ...)))

    ((_ b #(mod program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (modulo n1 n2) stack ...)))

    ((_ b #(neg program ...) '(n stack ...))
     (interpret b ev #(program ...) (list (- n) stack ...)))


    ((_ b #(= program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (if (= n1 n2) -1 0) stack ...)))

    ((_ b #(> program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (if (> n1 n2) -1 0) stack ...)))

    ((_ b #(< program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...) (list (if (< n1 n2) -1 0) stack ...)))


    ((_ b #(not program ...) '(n stack ...))
     (interpret b ev #(program ...)
                (list (if (= n 0) -1 0) stack ...)))

    ((_ b #(and program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...)
                (list (if (or (= n2 0) (= n1 0)) 0 -1) stack ...)))

    ((_ b #(or program ...) '(n2 n1 stack ...))
     (interpret b ev #(program ...)
                (list (if (and (= n2 0) (= n1 0)) 0 -1) stack ...)))


    ((_ b #(drop program ...) '(n1 stack ...))
     (interpret b #(program ...) '(stack ...)))

    ((_ b #(swap program ...) '(n2 n1 stack ...))
     (interpret b #(program ...) '(n1 n2 stack ...)))

    ((_ b #(dup program ...) '(n1 stack ...))
     (interpret b #(program ...) '(n1 n1 stack ...)))

    ((_ b #(over program ...) '(n2 n1 stack ...))
     (interpret b #(program ...) '(n1 n2 n1 stack ...)))

    ((_ b #(rot program ...) '(n3 n2 n1 stack ...))
     (interpret b #(program ...) '(n1 n2 n3 stack ...)))

    ((_ b #(depth program ...) stack)
     (interpret b ev #(program ...) (cons (length stack) stack)))

    
    ((_ b #(define word program ...) stack)
     (let loop ((body '())
                (prog '(program ...)))
       (if (equal? (car prog) 'end)
           (eval `(interpret
                   ,(cons (list (symbol->string 'word)
                                (list->vector (reverse body)))
                          'b)
                   ev ,(list->vector (cdr prog)) stack)
                 (interaction-environment))
           (loop (cons (car prog) body) (cdr prog)))))

    ((_ b #(exit program ...) stack) stack)

    ((_ b #(if program ...) '(flag stack ...))
     (interpret b ev (if (= flag 0)
                         (list->vector (member 'endif '(program ...)))
                         #(program ...))
                '(stack ...)))

    ((_ b #(endif program ...) stack) (interpret b #(program ...) stack))


    ((_ b #(word program ...) stack)
     (interpret b ev #(program ...) 
                (if (integer? 'word)
                    (cons word stack)
                    (interpret b ev
                               (cadr (assoc (symbol->string 'word) 'b))
                               stack))))
    ))