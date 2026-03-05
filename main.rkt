#lang racket



;batch mode dectection

(define interactive?
  (let [(args (current-command-line-arguments))]
    (not (or (vector-member "-b" args)
             (vector-member "--batch" args)))))



;skip spaces 

(define (skip-space lst)
  (cond
    [(not (list? lst)) #f]
    [(null? lst) '()]
    [(char-whitespace? (car lst)) (skip-space (cdr lst))]
    [else lst]))




; read into a string

(define (read-number lst acc)
  (cond
    [(null? lst) (list (list->string (reverse acc)) '())]
    [(char-numeric? (car lst)) 

     (read-number (cdr lst) (cons (car lst) acc))]

    [else (list (list->string (reverse acc)) lst)]))

