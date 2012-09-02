; SICP Exercise 2.42
;   Dawie de Klerk
;   2012-09-01

(define (enumerate-interval low high)
  (if (> low high) 
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (append-map
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; Constuctors and selectors for the board
(define empty-board '())
(define (adjoin-position row col board)
  ;; A board is represented by a list of pairs (col . row)
  (cons (make-pos col row) board))
(define (get-queen k board)
  ;; Get the queen on column k
  (car (filter (lambda (q) (= (col q) k))
               board)))
(define (remove-queen k board)
  ;; Get the board without the queen on column k
  (filter (lambda (q) (not (= (col q) k)))
          board))

;;; Constuctors and selectors for the positions
(define (make-pos col row)
  (cons col row))
(define (col pos)
  (car pos))
(define (row pos)
  (cdr pos))

;;; Predicates
(define (safe? k positions)
  ;; The queen on column k is safe if there are no other queens
  ;; that checks it
  (zero? (length
     (filter (lambda (q) 
               (queen-check? (get-queen k positions) q))
             (remove-queen k positions)))))

(define (queen-check? queen other)
  (let ((qx (col queen))
        (qy (row queen))
        (ox (col other))
        (oy (row other)))
    (or
      (= qx ox)
      (= qy oy)
      (= (abs (- qx ox)) (abs (- qy oy))))))

(define (print-sol board)
  (let ((board-size (apply max (map col board))))
    (define (print-row pos)
      (define (iter-row c)
        (if (> c 0)
          (begin 
            (if (= (col pos) c)
                (display "|Q")
                (display "| "))
              (iter-row (- c 1)))))
      (iter-row board-size)
      (display "|")
      (newline))
    (for-each print-row
         (sort board 
               (lambda (p1 p2) (< (row p1) (row p2)))))
    (newline)
    (newline)))

