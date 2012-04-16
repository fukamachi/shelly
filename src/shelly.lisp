#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly
  (:use :cl)
  (:shadow :read :eval)
  (:import-from :swank-backend
                :quit-lisp
                :arglist)
  (:import-from :cl-fad
                :file-exists-p))
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
        do (interpret expr)
           (prompt)
        finally (quit-lisp)))

(defun shelly::eval (expr)
  #-clisp (princ (cl:eval expr))
  #+clisp (cl:eval expr))

(defun shelly::read (expr)
  (destructuring-bind (fn &rest args) expr
    (cons fn
          (mapcar #'(lambda (a)
                      (let ((a (canonicalize-arg a)))
                        (if (and (not (keywordp a))
                                 (symbolp a))
                            (string a)
                            a)))
                  args))))

@export
(defun shelly::interpret (expr)
  (etypecase expr
    (string (shelly::interpret (read-from-string (format nil "(~A)" expr))))
    (list
     (let ((expr (shelly::read expr)))
       (handler-case (shelly::eval expr)
         (program-error ()
           (print-usage (car expr)))
         (undefined-function ()
           (format *error-output* "Error: command not found: ~(~A~)" (car expr)))
         (t (c)
           (format *error-output* "Error: ~A" c)))))))

(defun print-usage (fn)
  (format t
          "Usage: ~(~A~) [~{~(~A~^ ~)~}]"
          fn
          (swank-backend:arglist fn)))

(defun canonicalize-arg (arg)
  (cond
    ((numberp arg) arg)
    ((string= "--" (handler-case (subseq (string arg) 0 2)
                     (simple-error ())))
     (concatenate 'string ":" (subseq (string arg) 2)))
    ((fad:file-exists-p (string arg)))
    (t arg)))
