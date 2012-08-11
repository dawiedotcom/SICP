

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


