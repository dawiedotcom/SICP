; SICP Exersice 3.7
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (make-account balance passwd)
  (let ((passwds (list passwd)))
    ;; Private functions
    (define (check-passwd pw)
      (pair? (memq pw passwds)))
    ;; Public functions
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (add-passwd new-pw)
      (set! passwds (cons new-pw passwds)))
    ;; Dispatch calls
    (define (dispatch pw m)
      (if (check-passwd pw)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'add-passwd) add-passwd)
                (else
                 (error "MAKE-ACCOUNT: Unkown request" m)))
          (lambda args "Incorrect password")))
    dispatch))

(define (make-joint account current-pw new-pw)
  ((account current-pw 'add-passwd) new-pw)
  account)

(define (do-examples)
  (let ((peter-acc (make-account 100 'open-sesame)))
    (println "(define peter-acc\n  (make-account 100 'open-sesame))")
    (print-eval ((peter-acc 'open-sesame 'withdraw) 10))
    (let ((paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
      (println "(define paul-acc\n  (make-joint peter-acc 'open-sesame 'rosebud))")
      (print-eval ((paul-acc 'rosebud 'withdraw) 10))
      (print-eval ((peter-acc 'open-sesame 'withdraw) 10)))))
