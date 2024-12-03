#lang racket/base

(require racket/list
         racket/string)

(define reports
  (call-with-input-file "day-02.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (map string->number (string-split line " "))))))

(define (safe? r)
  (define all-increasing
    (for/and ([n0 (in-list r)]
              [n1 (in-list (cdr r))])
      (> n1 n0)))
  (define all-decreasing
    (for/and ([n0 (in-list r)]
              [n1 (in-list (cdr r))])
      (< n1 n0)))
  (and (or all-increasing all-decreasing)
       (for/and ([n0 (in-list r)]
                 [n1 (in-list (cdr r))])
         (< (abs (- n1 n0)) 4))))

(define part1
  (for/sum ([r (in-list reports)]
            #:when (safe? r))
    1))

(define (dampen r)
  (cons
   r
   (for/list ([idx (in-range 0 (length r))])
     (append
      (take r idx)
      (drop r (add1 idx))))))

(define part2
  (for/sum ([r (in-list reports)]
            #:when (for/or ([dr (in-list (dampen r))])
                     (safe? dr)))
    1))
