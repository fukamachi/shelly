#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.core
  (:use :cl)
  (:shadow :read :read-from-string :print)
  (:import-from :swank-backend
                :arglist)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :bordeaux-threads
                :all-threads
                :thread-alive-p
                :join-thread)
  (:import-from :shelly.impl
                :condition-undefined-function-name)
  (:import-from :shelly.error
                :shelly-error
                :shelly-read-error
                :shelly-command-not-found-error)
  (:import-from :shelly.util
                :terminate)
  (:import-from :shelly.commands
                :*shelly-commands-package*
                :*shelly-args*))
(in-package :shelly.core)

(cl-annot:enable-annot-syntax)

(defun shelly.core::read-from-string (string)
  (let ((*package* (find-package *shelly-commands-package*)))
    (cl:read-from-string string)))

@export
(defun shelly.core::read (expr)
  (destructuring-bind (fn &rest args) expr
    (cons (handler-case (if (stringp fn)
                            (read-from-string fn)
                            fn)
            (error (c)
              (error 'shelly-read-error
                     :expression expr
                     :format-control (princ-to-string c))))
          (mapcar #'canonicalize-arg
                  args))))

(defun shelly.core::print (result)
  (typecase result
    (string (princ result))
    (T (pprint result))))

@export
(defun interpret (expr &key verbose)
  (setf *shelly-args* expr)
  (when verbose
    (format *debug-io* "~&;-> ~S~%" expr))

  (handler-case
      (let ((expr (shelly.core::read expr))
            (system-threads #+thread-support (bt:all-threads)
                            #-thread-support nil))
        (labels ((alive-user-threads ()
                   (remove-if-not #'bt:thread-alive-p
                                  (set-difference
                                   #+thread-support (bt:all-threads)
                                   #-thread-support nil
                                   system-threads)))
                 (wait-user-threads ()
                   (let
                       #+ccl ((ccl::*invoke-debugger-hook-on-interrupt* t)
                              (*debugger-hook* (lambda () (ccl:quit))))
                       #-ccl ()
                       (map nil #'bt:join-thread (alive-user-threads)))))
          (when verbose
            (format *debug-io* "~&;-> ~S~%" expr))

          (let ((result
                 (multiple-value-list
                  (handler-case (let ((*package* (find-package *shelly-commands-package*)))
                                  (eval expr))
                    (program-error ()
                      (print-usage (car expr))
                      (values))
                    (undefined-function (c)
                      (let ((funcname (condition-undefined-function-name c)))
                        (if (string-equal funcname (car expr))
                            (error 'shelly-command-not-found-error
                                   :command funcname)
                            (error c)))
                      (values))))))
            (when result
              (shelly.core::print (car result))))

          (fresh-line)

          (handler-case (wait-user-threads)
            (condition () nil))))
    (error (c)
      (format *error-output* "Error: ~A" c)
      (terminate 1))))

(defun prompt ()
  (fresh-line)
  (princ "> ")
  (force-output))

@export
(defun run-repl (&key verbose)
  "Start Read-Eval-Print Loop for interactive execution."
  (prompt)
  (loop for expr = (read-line *terminal-io* nil :eof)
        until (eq expr :eof)
        do (unwind-protect
               (unless (string= "" expr)
                 (interpret
                  (mapcar #'prin1-to-string
                          (read-from-string (concatenate 'string "(" expr ")")))
                  :verbose verbose))
             (run-repl))
           (prompt)
        finally (terminalte)))

(defun canonicalize-arg (arg0)
  (unless (stringp arg0)
    (return-from canonicalize-arg arg0))

  (let ((arg (handler-case (read-from-string arg0)
                (error () arg0))))
    (cond
      ((or (numberp arg) (consp arg) (typep arg 'boolean))
       arg)
      ((string= "" arg) arg)
      ((string= "--" (handler-case (subseq (string arg) 0 2)
                       (simple-error ())))
       (intern (subseq (string arg) 2)
               :keyword))
      ((ignore-errors (fad:file-exists-p arg0)))
      ((and (not (keywordp arg))
            (symbolp arg)
            (string= (package-name (symbol-package arg)) *shelly-commands-package*))
       (string arg0))
      (t arg))))

(defun print-usage (fn)
  (if (symbolp fn)
      (format t
              "~&Usage: ~(~A~) [~{~(~A~^ ~)~}]~%"
              fn
              (swank-backend:arglist fn))
      (error 'shelly-error
             :format-control "Invalid command \"~S\""
             :format-arguments (list fn))))
