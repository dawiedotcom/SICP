

(define print
  ;; Print the arguments seperated by a space.
  (lambda args
    (if (= (length args) 1) 
        (display (car args))
        (begin (display (car args))
               (display " ")
               (apply print (cdr args))))))

(define println
  ;; Print followed by a newline.
  (lambda args 
	(apply print args)
	(newline)))

(define ->
  ;; Threads val through a list of functions
  (lambda (val . funcs)
    (if (= (length funcs) 0) 
        val
        (apply -> (cons ((car funcs) val) 
						(cdr funcs))))))

(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (curry-map f lst)
  ;; Partially apply each element in lst to f.
  ;; f(x, y) with '(1 2 3) will return
  ;; (f(1, y) f(2, y) f(3, y)
  (map (lambda (x) 
         (lambda xs (apply f (cons x xs))))
       lst))

(define (curry-map-all fs lst)
  ;; Apply curry-map to each fs and return a
  ;; flat list of partially applied functions.
  ;; (f(x, y), g(x, y)) on (1, 2) returns
  ;; (f(1,y), f(2,y), g(1,y), g(2,y))
  (fold (lambda (f acc)
          (append acc (curry-map f lst)))
        '()
        fs))

(define (carthesian-map f . lists)
  ;; Applies f to all the possible combinations
  ;; of elements in the lists passed as other 
  ;; arguments.
  (map apply
       (fold 
         (lambda (lst acc)
           (curry-map-all acc lst)) 
         (list f)
         lists)))

(define-syntax print-eval 
  ;; Prints a form, then evaluates and prints the result
  (syntax-rules () 
    ((_ e) (begin 
             (println (quote e))
             (println ";=" e)))))
