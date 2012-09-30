; SICP Exersice 3.3
;   Dawie de Klerk
;   2012-09-30

(load "../utils.scm")

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? passwd pw)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else
               (error "MAKE-ACCOUNT: Unkown request" m)))
        (lambda args "Incorrect password")))
  dispatch)

(define (do-example)
  (let ((acc (make-account 100 'secret-password)))
    (println "(define acc (make-account 100 'secret-password))")
    (print-eval ((acc 'secret-password 'withdraw) 40))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    'done))
