#lang racket/base

(require racket/match)

(define-values (lhss rhss)
  (call-with-input-file "day-01.txt"
    (lambda (in)
      (for/lists (lhss rhss #:result (values (sort lhss <)
                                             (sort rhss <)))
                 ([line (in-lines in)])
        (match-define (regexp #rx"([^ ]+) +([^ ]+)"
                              (list _
                                    (app string->number lhs)
                                    (app string->number rhs)))
          line)
        (values lhs rhs)))))

(define part1
  (for/sum ([lhs (in-list lhss)]
            [rhs (in-list rhss)])
    (abs (- lhs rhs))))

(define part2
  (for*/sum ([id (in-list lhss)]
             [n  (in-list rhss)]
             #:when (= id n))
    n))
