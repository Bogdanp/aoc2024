#lang racket/base

(require racket/match)

(define bytes->number
  (compose1 string->number bytes->string/utf-8))

(define part1
  (call-with-input-file "day-03.txt"
    (lambda (in)
      (define (select m)
        (match-define (list _ lhs rhs) m)
        (* (bytes->number lhs)
           (bytes->number rhs)))
      (for/sum ([n (in-list (regexp-match*
                             #:match-select select
                             #rx"mul\\(([0-9]+),([0-9]+)\\)" in))])
        n))))

(define part2
  (call-with-input-file "day-03.txt"
    (lambda (in)
      (define (select m)
        (match-define (list _ op _ lhs rhs) m)
        (case op
          [(#"do") 'on]
          [(#"don't") 'off]
          [else (* (bytes->number lhs)
                   (bytes->number rhs))]))
      (for/fold ([sum 0]
                 [mul 1]
                 #:result sum)
                ([n (in-list (regexp-match*
                              #:match-select select
                              #rx"(do|don't|mul)\\((([0-9]+),([0-9]+))?\\)" in))])
        (case n
          [(on)  (values sum 1)]
          [(off) (values sum 0)]
          [else  (values (+ sum (* mul n)) mul)])))))
