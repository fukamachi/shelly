#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.util
  (:use :cl)
  (:import-from :cl-fad
                :file-exists-p
                :walk-directory
                :copy-file))
(in-package :shelly.util)

(cl-annot:enable-annot-syntax)

@export
(defun shadowing-use-package (packages-to-use &optional (package *package*))
  (let ((packages-to-use (if (consp packages-to-use)
                             packages-to-use
                             (list packages-to-use))))
    (dolist (package-to-use packages-to-use)
      (do-external-symbols (symbol package-to-use)
        (shadowing-import symbol package)))))

@export
(defun load-systems (systems)
  #+quicklisp (ql:quickload systems :verbose nil :prompt nil)
  #-quicklisp (dolist (system systems) (asdf:load-system system :verbose nil))
  (shadowing-use-package
   (remove-if-not #'find-package
                  (if (consp systems)
                      systems
                      (list systems)))))

@export
(defun check-version (version)
  (let ((*standard-output* (make-broadcast-stream)))
    (unless (string= version (slot-value (asdf:find-system :shelly)
                                         'asdf:version))
      (format *error-output*
              "Warning: different version of Shelly was detected. Try \"shly install\".~%")
      (force-output *error-output*))
    (values)))

@export
(defun load-shlyfile (&optional (shlyfile  #P"shlyfile"))
  (when (file-exists-p shlyfile)
    (pushnew (directory-namestring (asdf::truenamize shlyfile))
             asdf:*central-registry*)
    (let ((*standard-output* (make-broadcast-stream))
          #+quicklisp (original-quickload #'ql:quickload))
      (flet (#+quicklisp
             (ql:quickload (systems &rest args)
              (apply #'original-quickload systems args)))
        (load shlyfile)))))

@export
(defun copy-directory (from to &key overwrite)
  (let ((len (length (namestring (truename from)))))
    (fad:walk-directory
     from
     (lambda (x)
       (fad:copy-file
        x
        (ensure-directories-exist
         (merge-pathnames
          (subseq (namestring x) len)
          to))
        :overwrite overwrite)))))
