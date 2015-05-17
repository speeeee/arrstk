#lang racket/base
(require racket/list)

(struct v (val type))
(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define (write-spec ls) 
  (if (list? ls) (begin (display "(") (map write-spec ls) (display ") "))
      (if (v? ls) (printf "(v ~a ~a)" (v-val ls) (v-type ls)) (write ls))))

(define (string-split-spec str)
  (filter (λ (x) (not (empty? (string->list x)))) (splt str '())))
  ;(splt str '()))
(define (splt str lst)
  (if (empty? (string->list str)) lst
      (splt (cadr (tok (string->list str) '())) (append lst (list (car (tok (string->list str) '())))))))

(define (tok str lst)
  (if (empty? str) (list (list->string lst) "")
    (let ([c (car str)])
      (if (and (not (empty? lst)) (equal? (car lst) #\"))
          (if (equal? c #\") (list (list->string (append lst (list c))) (list->string (cdr str)))
              (tok (cdr str) (append lst (list c))))
          (if (or (char-whitespace? c)) (if (empty? lst) (tok (cdr str) lst) (list (list->string lst) (list->string str)))
              (tok (cdr str) (append lst (list c))))))))

(define (strcar s) (car (string->list s)))

(define (lex l)
  (cond [(or (char-numeric? (strcar l)) (char=? (strcar l) #\.)) (v l 'Int)]
        [(char=? (strcar l) #\") (v l 'String)]
        [else (v l 'Sym)]))

(define (push~ stk s)
  (cond [(equal? (v-val s) "|") (push stk '())]
        [else (push (ret-pop stk) (push (pop stk) s))]))
(define (process stk n)
  (if (empty? stk) n (process (cdr stk) (push~ n (car stk)))))

(define (main)
  (write-spec (process (map lex (string-split-spec (read-line))) '(())))
  (main))

(main)