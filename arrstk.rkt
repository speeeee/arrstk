#lang racket/base
(require racket/list)

(struct v (val type))
(struct arr (len type)) ; for return arrays pushed to the stack.
(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define funs* (list (list "+" (list (arr 1 "Int")) (list (arr "a" "Int")))))

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

(define (call-fun f stk)
  (if (not (member (v-val f) (map car funs*))) (printf "ERROR: function `~a' does not exist." (v-val f))
      (let* ([fn (findf (λ (x) (equal? (car x) (v-val f))) funs*)] [sub (take (reverse stk) (length (third fn)))])
        (displayln sub)
        (if (member #f (map (λ (x y) (equal? (arr-type x) (v-type (car y)))) (third fn) sub)) (printf "ERROR: type mismatch.")
            (append (take stk (- (length stk) (length sub))) (second fn))))))

(define (lex l)
  (cond [(or (char-numeric? (strcar l)) (char=? (strcar l) #\.)) (v l "Int")]
        [(char=? (strcar l) #\") (v l "String")]
        [(char=? (strcar l) #\#) (v l 'Type)]
        [else (v l 'Sym)]))

(define (push~ stk s)
  (cond [(equal? (v-val s) "|") (push stk '())]
        [(equal? (v-type s) 'Sym) (call-fun s stk)]
        [else (push (ret-pop stk) (push (pop stk) s))]))
(define (process stk n)
  (if (empty? stk) n (process (cdr stk) (push~ n (car stk)))))

(define (main)
  (write-spec (process (map lex (string-split-spec (read-line))) '(())))
  (main))

(main)