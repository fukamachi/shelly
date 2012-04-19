#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.core
  (:use :cl
        :split-sequence)
  (:shadow :read :eval)
  (:import-from :swank-backend
                :quit-lisp
                :arglist)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :shelly.impl
                :condition-undefined-function-name))
(in-package :shelly.core)

(cl-annot:enable-annot-syntax)

(defun shelly.core::eval (expr)
  #-clisp (pprint (cl:eval expr))
  #+clisp (cl:eval expr))

(defun shelly.core::read (expr)
  (destructuring-bind (fn &rest args) expr
    (cons (handler-case (read-from-string fn)
            (error (c) (format t "Read-time error: ~A~%~A"
                               expr c)))
          (mapcar #'(lambda (a)
                      (let* ((a (canonicalize-arg a)))
                        (if (and (not (keywordp a))
                                 (not (typep a 'boolean))
                                 (symbolp a))
                            (string a)
                            a)))
                  args))))

(defun interpret (&rest expr)
  (let ((expr (shelly.core::read expr)))
    (handler-case (shelly.core::eval expr)
      (program-error ()
        (print-usage (car expr)))
      (undefined-function (c)
        (format *error-output* "Error: command not found: ~(~A~)"
                (condition-undefined-function-name c)))
      (error (c)
        (format *error-output* "Error: ~A" c)))))

(defun prompt ()
  (fresh-line)
  (princ "> ")
  (force-output))

@export
(defun run-repl ()
  (prompt)
  (loop for expr = (read-line *terminal-io* nil :eof)
        until (eq expr :eof)
        do (unless (string= "" expr)
             (apply #'interpret (split-sequence #\Space expr)))
           (prompt)
        finally (quit-lisp)))

(defun canonicalize-arg (arg0)
  (let ((arg (handler-case (read-from-string arg0)
               ((or reader-error package-error end-of-file) ()
                 arg0))))
    (cond
      ((or (numberp arg) (consp arg) (typep arg 'boolean))
       arg)
      ((string= "" arg) arg)
      ((string= "--" (handler-case (subseq (string arg) 0 2)
                       (simple-error ())))
       (intern (subseq (string arg) 2)
               :keyword))
      ((fad:file-exists-p (string arg)))
      ((and (not (keywordp arg)) (symbolp arg))
       (string arg0))
      (t arg))))

(defun print-usage (fn)
  (format t
          "Usage: ~(~A~) [~{~(~A~^ ~)~}]"
          fn
          (swank-backend:arglist fn)))