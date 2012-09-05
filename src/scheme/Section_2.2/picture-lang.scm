; SICP Exercises 2.45 to 2.52
;   Dawie de Klerk
;   2012-09-04


;;;; VECTORS
;;; Representation of 2d vectors in terms of pairs
(define (make-vect x y)
  (cons x y))

;;; Vector selectors
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

;;; Vector operations
(define (add-vect v u)
  (make-vect (+ (xcor-vect v)
                (xcor-vect u))
             (+ (ycor-vect v)
                (ycor-vect u))))
(define (sub-vect v u)
  (add-vect v (scale-vect -1 u)))
(define (scale-vect a v)
  (make-vect (* a (xcor-vect v))
             (* a (ycor-vect v))))


;;;; FRAMES
;;; Representation of frames in terms of lists
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (car (cdr f)))
(define (edge2-frame f)
  (list-ref f 2))

;;; Representation of frames in terms of pairs
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (edge2-frame f)
  (cdr (cdr f)))

;;; Frame operations
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;;;; SEGMENTS
;;; Segements as a pair of vectors
;(define (make-segment xstart ystart xend yend)
;  (cons (make-vect xstart ystart)
;        (make-vect xend yend)))
(define (make-segment v1 v2)
  (cons v1 v2))


(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;;; Segments operations
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (turtle->segments v1 v2 . vs)
  (fold-left 
    (lambda (a n) 
      (cons 
        (make-segment (end-segment (car a)) n)
        a)) 
    (list (make-segment v1 v2))
    vs))


;;;; PAINTERS

(define (outline-painter frame)
  (let ((bl (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    ((segments->painter 
       (turtle->segments bl br tr tl bl)) 
     frame)))

(define (diamond-painter frame)
  (let ((bottom (make-vect 0.5 0))
        (right (make-vect 1 0.5))
        (top (make-vect 0.5 1 ))
        (left (make-vect 0 0.5)))
    ((segments->painter 
       (turtle->segments bottom right top left bottom)) 
     frame)))

(define (triangle-painter frame)
  (let ((h (/ (sqrt 3.) 2)))
    ((segments->painter
       (turtle->segments
         (make-vect 0 0)
         (make-vect 1 0)
         (make-vect 0.5 h)
         (make-vect 0 0)))
     frame)))

(define (x-painter frame)
  ((segments->painter 
     (list (make-segment 
             (make-vect 0 0)
             (make-vect 1 1))
           (make-segment 
             (make-vect 1 0)
             (make-vect 0 1))))
   frame))

(define (wave frame)
  ((segments->painter
     (append 
       (turtle->segments
         (make-vect 0.4  1)
         (make-vect 0.35 0.8)
         (make-vect 0.4  0.6)
         (make-vect 0.3  0.6)
         (make-vect 0.15 0.55)
         (make-vect 0.0  0.8))
       (turtle->segments
         (make-vect 0.0 0.6)
         (make-vect 0.15 0.45)
         (make-vect 0.3  0.55)
         (make-vect 0.32 0.5)
         (make-vect 0.3  0))
       (turtle->segments
         (make-vect 0.4 0)
         (make-vect 0.5 0.3)
         (make-vect 0.6 0))
       (turtle->segments
         (make-vect 0.7 0)
         (make-vect 0.65 0.5)
         (make-vect 1 0.2))
       (turtle->segments
         (make-vect 1 0.4)
         ;(make-vect 0.65 0.6)
         (make-vect 0.7 0.6)
         (make-vect 0.6 0.6)
         (make-vect 0.65 0.8)
         (make-vect 0.6 1))))
   frame))


;;;; PAINTER TRANSFORMATIONS

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0. 1.)
                     (make-vect 1. 1.)
                     (make-vect 0. 0.)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1. 0.)
                     (make-vect 0. 0.)
                     (make-vect 1. 1.)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1. 0.)
                     (make-vect 1. 1.)
                     (make-vect 0. 0.)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (scale-painter a painter)
  (transform-painter painter
                     (make-vect 0. 0.)
                     (make-vect a  0.)
                     (make-vect 0. a)))

(define (shift-painter v painter)
  (transform-painter painter
                     v
                     (add-vect v (make-vect 1. 0.))
                     (add-vect v (make-vect 0. 1.))))


(define (sharpinski-split painter n)
  (define (iter p)
    (let ((small-painter (scale-painter 0.5 p)))
      (define (new-frame x y f)
        ((shift-painter (make-vect x y) 
                        small-painter) f))
      (lambda (frame)
        (new-frame 0.0  0.0  frame)
        (new-frame 0.5  0.0  frame)
        (new-frame 0.25 (/ (sqrt 3.) 4)  frame))))
  (if (zero? n)
    painter
    (sharpinski (iter painter) (+ n -1))))

(define (draw-sharpinski-triangle n)
  ((flip-vert (sharpinski-split triangle-painter n)) eye-frame))
;;;; GRAPHICS

(define gfx-dev (make-graphics-device #f))

(define (graphics-reset)
  (graphics-clear gfx-dev)
  (graphics-set-coordinate-limits 
    gfx-dev 
    -0.1 -0.1 
    1.1 1.1))
(graphics-reset)

(define (draw-line p1 p2)
  (graphics-draw-line 
    gfx-dev 
    (xcor-vect p1)
    (ycor-vect p1)
    (xcor-vect p2)
    (ycor-vect p2)))

(define eye-frame 
  (make-frame (make-vect 0 0) 
              (make-vect 1 0) 
              (make-vect 0 1)))
