#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly
  (:use :cl)
  (:import-from :swank-backend
                :quit-lisp
                :arglist))
(in-package :shelly)

(cl-annot:enable-annot-syntax)

(defun prompt ()
  (fresh-line)
  (princ "> ")
  (force-output))

@export
(defun run-repl ()
  (prompt)
  (loop for expr = (read-line *terminal-io* nil :eof)
        until (eq expr :eof)
        do (shelly-eval expr)
           (prompt)
        finally (quit-lisp)))

(defun shelly-eval (expr)
  #-clisp (princ (eval expr))
  #+clisp (eval expr))

(defun shelly-read (expr-string)
  ;; TODO: parse the expression.
  (read-from-string (format nil "(~A)" expr-string)))

@export
(defun shelly-interpret (expr-string)
  (let ((expr (shelly-read expr-string)))
    (handler-case (shelly-eval expr)
      (program-error ()
        (print-usage (car expr)))
      (undefined-function ()
        (format *error-output* "Error: command not found: ~(~A~)" (car expr)))
      (t (c)
        (format *error-output* "Error: ~A" c)))))

(defun print-usage (fn)
  (format t
          "Usage: ~(~A~) [~{~(~A~^ ~)~}]"
          fn
          (swank-backend:arglist fn)))
