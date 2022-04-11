;DFA
;08-04-2022
;Octavio Fenollosa A01781042

#lang racket

(provide arithmetic-lexer)

(define (validate-string input-string dfa)
    " Determine if the input string is accepted by the dfa "

    (let loop
        ([lst (string->list input-string)] ; Primer elemento de la lista
            [transition (car dfa)] ; Segundo elemento de la lista
            [state (cadr dfa)] ; Tercer elemento de la lista
            [item-list empty]  ; Lista de Elementos
            [token-list empty] ; Tipos de tokens
        )

        (if (empty? lst)
            (if (member state (caddr dfa)) 
            ; Regresa lista de tokens
                (if (equal? state 'blank-after-var)
                    token-list
                    (append token-list (list (list (process item-list) state)))
                )#f
            )

            (let-values
                ([(state token-type item) (transition state (car lst))])
            
            ; Recursiva
                (loop
                    (cdr lst)
                    transition
                    state
                    (if (char-blank? item)
                        empty
                        (if token-type
                            (list item)
                            (append item-list (list item))
                        )
                    )

                    (if token-type
                        (append token-list (list (list (process item-list) token-type)))
                        token-list
                    )
                )
            )
        )
    )
)

(define (accept-simple-arithmetic-with-type state symbol)
    " Transition function that accepts arithmetic
    expressions with decimal point. Acceptance states:
        * 'int
        * 'float
        * 'space
        * 'variable
        * 'parenthesis"

    (let
        (
            [operator (list #\+ #\- #\* #\/ #\^ #\=)]
            [parenthesis (list #\( #\) )]
        )

        (cond
            [(eq? state 'q0) (cond ; char 
                [(char-numeric? symbol) (values 'int #f symbol)] ; espacio o tab
                [(char-blank? symbol) (values 'q0 #f symbol)] ; negativos
                [(eq? symbol #\-) (values 'int #f symbol)] ; variables
                [(char-alphabetic? symbol) (values 'variable #f symbol)] ; parentesis
                [(member symbol parenthesis) (values 'parenthesis #f symbol)] ; decimales
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'parenthesis) (cond ; char  
                [(char-numeric? symbol) (values 'int 'parenthesis symbol)] ; espacio o tab
                [(char-blank? symbol) (values 'space 'parenthesis symbol)] ; negativos
                [(eq? symbol #\-) (values 'int #f symbol)] ; variables
                [(char-alphabetic? symbol) (values 'variable 'parenthesis symbol)] ; parentesis
                [(member symbol parenthesis) (values 'parenthesis 'parenthesis symbol)] ; punto decimal 
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'int) (cond ; Int input
                [(char-numeric? symbol) (values 'int #f symbol)] ;espacio o tab
                [(char-blank? symbol) (values 'blank-after-var 'int symbol)] ; operadores aritmeticos
                [(member symbol operator) (values 'operator 'int symbol)] ; decimales
                [(eq? symbol #\.) (values 'float #f symbol)] ; parentisis 
                [(member symbol parenthesis) (values 'parenthesis 'int symbol)] ; letras
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'float) (cond ; Numero despues del punto
                [(char-numeric? symbol) (values 'float #f symbol)]
                [(char-blank? symbol) (values 'blank-after-var 'float symbol)]
                [(member symbol operator) (values 'operator 'float symbol)]
                [(eq? symbol #\.) (values 'invalid 'float symbol)]
                [(member symbol parenthesis) (values 'parenthesis 'float symbol)]
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'space) (cond ; Espacio
                [(char-blank? symbol) (values 'space #f symbol)]
                [(member symbol operator) (values 'operator #f symbol)]
                [(eq? symbol #\.) (values 'invalid #f symbol)]
                [(char-numeric? symbol) (values 'int #f symbol)]
                [(char-alphabetic? symbol) (values 'variable #f symbol)]
                [(member symbol parenthesis) (values 'parenthesis #f symbol)]
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'operator) (cond ; Simbolo
                [(char-blank? symbol) (values 'blank-after-symbol 'operator symbol)]
                [(char-numeric? symbol) (values 'int 'operator symbol)]
                [(member symbol operator) (values 'invalid 'operator symbol)]
                [(eq? symbol #\.) (values 'invalid 'operator symbol)]
                [(char-alphabetic? symbol) (values 'variable 'operator symbol)]
                [(member symbol parenthesis) (values 'parenthesis 'operator symbol)]
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'blank-after-symbol) (cond
                [(char-blank? symbol) (values 'blank-after-symbol #f symbol)]
                [(member symbol operator) (values 'invalid #f symbol)]
                [(eq? symbol #\.) (values 'invalid #f symbol)]
                [(char-numeric? symbol) (values 'int #f symbol)]
                [(char-alphabetic? symbol) (values 'variable #f symbol)]
                [(member symbol parenthesis) (values 'parenthesis #f symbol)]
                [else (values 'invalid #f)]
            )]

            [(eq? state 'blank-after-var) (cond
                [(char-blank? symbol) (values 'blank-after-var #f symbol)]
                [(member symbol operator) (values 'operator #f symbol)]
                [(member symbol parenthesis) (values 'parenthesis #f symbol)]
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'variable) (cond
                [(char-numeric? symbol) (values 'variable #f symbol)]
                [(char-alphabetic? symbol) (values 'variable #f symbol)]
                [(eq? symbol #\_) (values 'variable #f symbol)]
                [(member symbol operator) (values 'operator 'variable symbol)]
                [(char-blank? symbol) (values 'blank-after-var 'variable symbol)]
                [(member symbol parenthesis) (values 'parenthesis 'variable symbol)]
                [else (values 'invalid #f symbol)]
            )]

            [(eq? state 'invalid) (values 'invalid #f symbol)]
        )
    )
)

(define (arithmetic-lexer str)
    (validate-string str (list accept-simple-arithmetic-with-type 'q0 (list 'int 'float 'space 'variable 'parenthesis 'blank-after-var
    )))
)
; Lista de strings
(define (process lst)
    (apply string-append
        (map (lambda (e)
            (if (char? e)
                (string e)
                (number->string e)))
        lst
        )
    )
)