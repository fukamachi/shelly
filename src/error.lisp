(in-package :cl-user)
(defpackage shelly.error
  (:use :cl)
  (:export :shelly-error
           :shelly-read-error
           :shelly-command-not-found-error))
(in-package :shelly.error)

(define-condition shelly-error (simple-error) ())

(define-condition shelly-read-error (shelly-error)
  ((expression :initarg :expression)))

(define-condition shelly-command-not-found-error (shelly-error)
  ((command :initarg :command))
  (:report
   (lambda (condition stream)
     (format stream "Command \"~A\" not found"
             (slot-value condition 'command)))))
