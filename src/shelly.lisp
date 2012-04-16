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

(defparameter *config-file*
              (format nil "# -*- mode: perl -*-

{
    default_lisp    => ~:[undef~;~:*\"~A\"~],
    implementations => {
        ~:*~:[~*~;~:*~A => \"~A\",~]
    },
}
"
                      (or
                       #+ccl "ccl"
                       #+sbcl "sbcl"
                       #+allegro "alisp"
                       #+clisp "clisp"
                       #+cmu "cmucl"
                       #+ecl "ecl")

                      (or
                       #+ccl (car ccl:*command-line-argument-list*)
                       #+sbcl (car sb-ext:*posix-argv*)
                       #+allegro (car (system:command-line-arguments))
                       #+clisp "clisp"
                       #+cmu (car ext:*command-line-strings*)
                       #+ecl (car (si:command-args)))))

@export
(defun install-script ()
  (let ((home-config-path
         (merge-pathnames ".shelly/" (user-homedir-pathname)))
        (shly-path
         (asdf:system-relative-pathname :shelly "bin/shly")))

    (ensure-directories-exist home-config-path)

    (with-open-file (out (merge-pathnames "config" home-config-path)
                         :direction :output)
      (write-string *config-file* out))

    (dolist (dir '("dumped-cores/" "bin/"))
      (ensure-directories-exist
       (merge-pathnames dir home-config-path)))

    (if (fad:file-exists-p shly-path)
        (fad:copy-file shly-path
         (merge-pathnames "bin/shly" home-config-path))
        (warn "Shelly script doesn't exist. Ignored.")))
  t)

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
