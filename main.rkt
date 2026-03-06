#lang racket



;batch mode dectection
(define prompt?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))



;skip spaces 

(define (skip-space lst)
  (cond
    [(not (list? lst)) #f]
    [(null? lst) '()]
    [(char-whitespace? (car lst)) (skip-space (cdr lst))]
    [else lst]))

;read string
(define (read-number lst acc)
  (cond
    [(null? lst) (list (list->string (reverse acc)) '())]
    [(char-numeric? (car lst)) 

     (read-number (cdr lst) (cons (car lst) acc))]

    [else (list (list->string (reverse acc)) lst)]))



(define (eval-expr chars hist)
  (let* [(trimmed (skip-space chars))]
    (cond
      [(null? trimmed) (list 'error "empty expression")]
      


      ;unary negation
      [(char=? (car trimmed) #\-)
       (let* [(arg-res (eval-expr (cdr trimmed) hist))]
         (if (eq? (car arg-res) 'error)
             arg-res
             (list (- (car arg-res)) (cadr arg-res))))]
             



      ;bin addition
      [(char=? (car trimmed) #\+)
       (let* [(left-res (eval-expr (cdr trimmed) hist))]
         (if (eq? (car left-res) 'error)
             left-res
             (let* [(right-res (eval-expr (cadr left-res) hist))]
               (if (eq? (car right-res) 'error)
                   right-res
                   (list (+ (car left-res) (car right-res)) (cadr right-res))))))]
                   


      ;bin multiplication
      [(char=? (car trimmed) #\*)
       (let* [(left-res (eval-expr (cdr trimmed) hist))]
         (if (eq? (car left-res) 'error)
             left-res
             (let* [(right-res (eval-expr (cadr left-res) hist))]
               (if (eq? (car right-res) 'error)
                   right-res
                   (list (* (car left-res) (car right-res)) (cadr right-res))))))]
                   

      ;bin division
      [(char=? (car trimmed) #\/)
       (let* [(left-res (eval-expr (cdr trimmed) hist))]
         (if (eq? (car left-res) 'error)
             left-res
             (let* [(right-res (eval-expr (cadr left-res) hist))]
               (cond
                 [(eq? (car right-res) 'error) right-res]
                 [(= (car right-res) 0) (list 'error "division by zero")] 
                 [else (list (quotient (car left-res) (car right-res)) (cadr right-res))]))))]
                 






      ;history 
      [(char=? (car trimmed) #\$)
       (let* [(num-res (read-number (cdr trimmed) '()))
              (id-str (car num-res))
              (rest-chars (cadr num-res))]
         (if (string=? id-str "")
             (list 'error "missing history id")
             (let* [(id (string->number id-str))
                    (rev-hist (reverse hist))] 
               (if (or (< id 1) (> id (length rev-hist)))
                   (list 'error "invalid history id")
                   (list (list-ref rev-hist (- id 1)) rest-chars)))))]
                   
      
      [(char-numeric? (car trimmed))
       (let* [(num-res (read-number trimmed '()))
              (val (string->number (car num-res)))
              (rest-chars (cadr num-res))]
         (list val rest-chars))]
         
      [else (list 'error "invalid character")])))


;quit and other stuffs
(define (repl hist)
  (if prompt? (display "> ") #f)
  (let [(input (read-line))] 
    (cond
      [(eof-object? input) #f]
      [(equal? input "quit") #f] 
      [else
       (let* [(chars (string->list input))
              (res-pair (eval-expr chars hist))
              (val (car res-pair))]
         (cond
           


           [(eq? val 'error)
            (display "Error: ") 
            (displayln (cadr res-pair))
            (repl hist)]
        
           [(not (null? (skip-space (cadr res-pair))))
            (displayln "Error: Invalid Expression")
            (repl hist)]
            
           [else
            (let* [(new-hist (cons val hist)) 
                   (id (length new-hist))]
              (display id)
              (display ": ")
              (displayln (real->double-flonum val)) 
              (repl new-hist))]))])))


(repl '())

