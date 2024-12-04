#lang racket/base

(require racket/match)

(define data
  (call-with-input-file "day-04.txt"
    (lambda (in)
      (for*/hash ([(line x) (in-indexed (in-lines in))]
                  [[char y] (in-indexed (in-string line))])
        (values (cons x y) char)))))

;; Accidentally solved a harder problem:
#;
(let ()
  (define (find-adjacent d c x y)
    (for*/list ([x (in-inclusive-range (sub1 x) (add1 x))]
                [y (in-inclusive-range (sub1 y) (add1 y))]
                #:when (eqv? (hash-ref d (cons x y) #f) c))
      (cons x y)))

  (define-values (graph starts)
    (for/fold ([g (hash)]
               [starts null])
              ([(pos c) (in-hash data)])
      (match-define (cons x y) pos)
      (define adjacent-positions
        (case c
          [(#\X) (find-adjacent data #\M x y)]
          [(#\M) (find-adjacent data #\A x y)]
          [(#\A) (find-adjacent data #\S x y)]
          [else null]))
      (define next-g
        (hash-update g pos (λ (poss) (append adjacent-positions poss)) null))
      (values next-g (if (eqv? c #\X)
                         (cons pos starts)
                         starts))))

  (define (get-char d x y)
    (hash-ref d (cons x y)))

  (define (get-poss g x y)
    (hash-ref g (cons x y)))

  (define (get-words d g sx sy)
    (define charss
      (let loop ([x sx] [y sy] [cs null])
        (define c (get-char d x y))
        (define poss (get-poss g x y))
        (if (null? poss)
            (list (reverse (cons c cs)))
            (apply
             append
             (for/list ([pos (in-list poss)])
               (match-define (cons x y) pos)
               (loop x y (cons c cs)))))))
    (for/list ([chars (in-list charss)])
      (apply string chars))))

(define directions
  '(((0  . +1) (0  . +1) (0  . +1) (0  . +1))
    ((0  . -1) (0  . -1) (0  . -1) (0  . -1))
    ((+1 .  0) (+1 .  0) (+1 .  0) (+1 .  0))
    ((-1 .  0) (-1 .  0) (-1 .  0) (-1 .  0))
    ((+1 . +1) (+1 . +1) (+1 . +1) (+1 . +1))
    ((-1 . -1) (-1 . -1) (-1 . -1) (-1 . -1))
    ((+1 . -1) (+1 . -1) (+1 . -1) (+1 . -1))
    ((-1 . +1) (-1 . +1) (-1 . +1) (-1 . +1))))

(define (+dir pos d)
  (match-define (cons x y) pos)
  (match-define (cons dx dy) d)
  (cons (+ dx x) (+ dy y)))

(define (get-words data pos)
  (for/list ([dir (in-list directions)])
    (for*/fold ([p pos] [cs null] #:result (apply string (reverse cs)))
               ([d (in-list dir)])
      #:break (not (hash-has-key? data p))
      (values (+dir p d) (cons (hash-ref data p) cs)))))

(define part1
  (for*/sum ([(pos c) (in-hash data)]
             #:when (eqv? c #\X)
             [w (in-list (get-words data pos))]
             #:when (equal? w "XMAS"))
    1))

(define (get-word data pos dirs)
  (for/fold ([chars null] #:result (apply string (reverse chars)))
            ([d (in-list dirs)])
    (cons (hash-ref data (+dir pos d) #\.) chars)))

(define (x-mas? data a-pos)
  (and
   (eqv? (hash-ref data a-pos) #\A)
   (member (get-word data a-pos '((-1 . -1) ( 0 .  0) (+1 . +1))) '("MAS" "SAM"))
   (member (get-word data a-pos '((-1 . +1) ( 0 .  0) (+1 . -1))) '("MAS" "SAM"))
   #t))

(define part2
  (for/sum ([(pos c) (in-hash data)]
            #:when (eqv? c #\A)
            #:when (x-mas? data pos))
    1))
