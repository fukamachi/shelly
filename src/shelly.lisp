#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly
  (:use :cl
        :split-sequence)
  (:shadow :read :eval)
  (:import-from :swank-backend
                :quit-lisp
                :arglist)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :osicat
                :file-permissions))
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
  #-clisp (pprint (cl:eval expr))
  #+clisp (cl:eval expr))

(defun shelly::read (expr)
  (etypecase expr
    (string (shelly::read (split-sequence #\Space expr)))
    (list
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
                     args))))))

@export
(defun shelly::interpret (&rest expr)
  (let ((expr (shelly::read expr)))
    (handler-case (shelly::eval expr)
      (program-error ()
        (print-usage (car expr)))
      (undefined-function (c)
        (format *error-output* "Error: command not found: ~(~A~)"
                (or
                 #+sbcl (slot-value c 'sb-kernel::name)
                 #+ecl (slot-value c 'si::name)
                 #+cmu (getf (conditions::condition-actual-initargs c) :name)
                 #+allegro (slot-value c 'excl::name)
                 #+ccl (slot-value c 'ccl::name)
                 #+clisp (slot-value c 'system::$name))))
      (error (c)
        (format *error-output* "Error: ~A" c)))))

(defvar *current-lisp-path*
    (or
     #+ccl (car ccl:*command-line-argument-list*)
     #+sbcl (car sb-ext:*posix-argv*)
     #+allegro (car (system:command-line-arguments))
     #+clisp "clisp"
     #+cmu (car ext:*command-line-strings*)
     #+ecl (car (si:command-args))))

(defvar *current-lisp-name*
    (or
     #+ccl "ccl"
     #+sbcl "sbcl"
     #+allegro "alisp"
     #+clisp "clisp"
     #+cmu "cmucl"
     #+ecl "ecl"))

(defvar *eval-option*
    (or
     #+ccl "--eval"
     #+sbcl "--eval"
     #+allegro "-e"
     #+clisp "-x"
     #+cmu "-eval"
     #+ecl "-eval"))

@export
(defparameter *shelly-version*
              (slot-value (asdf:find-system :shelly) 'asdf:version))

@export
(defvar *dumped-core-path*
    (merge-pathnames (format nil "dumped-cores/~A.core"
                             *current-lisp-name*)
                     (merge-pathnames ".shelly/" (user-homedir-pathname))))

@export
(defun install-script (&key quit-lisp)
  (let ((home-config-path
         (merge-pathnames ".shelly/" (user-homedir-pathname)))
        (shly-path
         (asdf:system-relative-pathname :shelly "bin/shly")))

    (ensure-directories-exist home-config-path)

    (with-open-file (out (merge-pathnames
                          (format nil "config.~A" *current-lisp-name*)
                          home-config-path)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out "# -*- mode: perl -*-

{
~:[~;~:*    binary_path => \"~A\",~]
}
"
              *current-lisp-path*))

    (with-open-file (out (merge-pathnames "config" home-config-path)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out "# -*- mode: perl -*-

{
    default_lisp => \"~A\",
    version => \"~A\",
}
"
              *current-lisp-name*
              (slot-value (asdf:find-system :shelly)
                          'asdf:version)))

    (dolist (dir '("dumped-cores/" "bin/"))
      (ensure-directories-exist
       (merge-pathnames dir home-config-path)))

    (cond
      ((fad:file-exists-p shly-path)
       (fad:copy-file shly-path
        (merge-pathnames "bin/shly" home-config-path)
        :overwrite t)
       (pushnew :user-exec
                (osicat:file-permissions
                 (merge-pathnames "bin/shly" home-config-path))))
      (t
       (warn "Shelly script doesn't exist. Ignored.")))

    (dump-core :quit-lisp nil))

  (format t "~&
Successfully installed!
Add this to your shell rc file (\".bashrc\", \".zshrc\" and so on).

    PATH=\"$HOME/.shelly/bin:$PATH\"

")
  (when quit-lisp
    (quit-lisp))
  (values))

@export
(defun dump-core (&key (quit-lisp t))
  (let ((command (dump-core-command *dumped-core-path*)))
    (unless command
      (princ "`dump-core' isn't supported on this implementation.")
      (return-from dump-core))

    (if quit-lisp
        (eval command)
        (asdf:run-shell-command "~A ~A '~A' ~A '~S'"
                                *current-lisp-path*
                                *eval-option*
                                "(ql:quickload :shelly)"
                                *eval-option*
                                command))))

@export
(defun rm-core ()
  (handler-case
      (progn (delete-file *dumped-core-path*)
             (format *standard-output*
                     "~&Successfully deleted: ~A~%"
                     *dumped-core-path*))
    (file-error (c) (princ c)))

  (quit-lisp))

(defun dump-core-command (filepath)
  #+allegro `(excl:dumplisp :name ,filepath)
  #+ccl `(ccl:save-application ,filepath)
  #+sbcl `(sb-ext:save-lisp-and-die ,filepath)
  #+clisp `(progn (ext:saveinitmem ,filepath) (ext:quit))
  #+cmu `(ext:save-lisp ,filepath :load-init-file nil)
  #-(or allegro ccl sbcl clisp cmu) nil)

(defun print-usage (fn)
  (format t
          "Usage: ~(~A~) [~{~(~A~^ ~)~}]"
          fn
          (swank-backend:arglist fn)))

@export
(defun shadowing-use-package (packages-to-use &optional (package *package*))
  (dolist (package-to-use packages-to-use)
    (do-external-symbols (symbol package-to-use)
      (shadowing-import symbol package))))

@export
(defun load-libraries (&rest libraries)
  (ql:quickload libraries)
  (shadowing-use-package libraries))

@export
(defun check-version (version)
  (let ((*standard-output* (make-broadcast-stream)))
    (unless (string= version *shelly-version*)
      (format *error-output*
              "Warning: different version of Shelly was detected. Try \\\"shly --install\\\".~%")
      (force-output *error-output*))
    (values)))

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
