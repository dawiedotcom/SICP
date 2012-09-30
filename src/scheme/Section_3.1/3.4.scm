; SICP Exersice 3.4
;   Dawie de Klerk
;   2012-09-30

(load "../utils.scm")

(define (call-the-cops)
  (println "Calling the cops.."))

(define (make-account balance passwd)
  (let ((wrong-count 0))
    ;; Account procedures
    (define (withdraw amount)
      ; Decrease the balance
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
               balance)
          "Insufficient funds"))
    (define (deposit amount)
      ; Increase the balance
      (set! balance (+ balance amount))
      balance)
    ;; Message dispatch if the password is correct
    (define (dispatch pw m)
      (if (eq? passwd pw)
          (begin ; then
            (set! wrong-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else
                    (error "MAKE-ACCOUNT: Unkown request" m))))
          (begin ; else
            (set! wrong-count (+ wrong-count 1))
            (if (= wrong-count 3)
                (call-the-cops))
            (lambda args "Incorrect password"))))
    dispatch))

(define (do-example)
  (let ((acc (make-account 100 'secret-password)))
    (println "(define acc (make-account 100 'secret-password))")
    (print-eval ((acc 'secret-password 'withdraw) 40))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    (print-eval ((acc 'secret-password 'deposit) 50))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    (print-eval ((acc 'some-other-passwd 'deposit) 50))
    'done))
