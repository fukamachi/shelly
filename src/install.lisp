#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.install
  (:use :cl)
  (:import-from :cl-fad
                :copy-file
                :file-exists-p)
  (:import-from :swank-backend
                :quit-lisp)
  (:import-from :shelly.impl
                :*current-lisp-name*
                :*current-lisp-path*
                :*eval-option*
                :save-core-image)
  (:import-from :shelly.util
                :shadowing-use-package))
(in-package :shelly.install)

(cl-annot:enable-annot-syntax)

@export
(defun install (&key quit-lisp)
  (ql:quickload :shelly)

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
       ;; XXX: must be more portable.
       (asdf:run-shell-command
        "chmod u+x ~A"
        (merge-pathnames "bin/shly" home-config-path)))
      (t
       (warn "Shelly script doesn't exist. Ignored.")))

    (dump-core :quit-lisp nil))

  (format t "~&
Successfully installed!
Add this to your shell rc file (\".bashrc\", \".zshrc\" and so on).

    PATH=$HOME/.shelly/bin:$PATH

")
  (when quit-lisp
    (quit-lisp))
  (values))

(defvar *dumped-core-path*
    (merge-pathnames (format nil "dumped-cores/~A.core"
                             *current-lisp-name*)
                     (merge-pathnames ".shelly/" (user-homedir-pathname))))

@export
(defun dump-core (&key (quit-lisp t))
  (cond
    (quit-lisp
     (ql:quickload :shelly)
     (shelly.util:shadowing-use-package :shelly))
    (T
     (asdf:run-shell-command "~A ~A '~A' ~A '~A' ~A '~S'"
      *current-lisp-path*
      *eval-option*
      "(ql:quickload :shelly)"
      *eval-option*
      "(shelly.util:shadowing-use-package :shelly)"
      *eval-option*
      '(save-core-image *dumped-core-path*)))))

@export
(defun rm-core ()
  (handler-case
      (progn (delete-file *dumped-core-path*)
             (format t "~&Successfully deleted: ~A~%"
                     *dumped-core-path*))
    (file-error (c) (princ c)))

  (quit-lisp))
