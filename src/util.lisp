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
                :copy-file)
  (:import-from :swank-backend
                :quit-lisp))
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
  (handler-case #+quicklisp (ql:quickload systems :verbose nil :prompt nil)
                #-quicklisp (dolist (system systems) (asdf:load-system system :verbose nil))
    (#+quicklisp ql::system-not-found #-quicklisp asdf:missing-component (c)
     (format *error-output* "~&Error: ~A~&" c)
     (swank-backend:quit-lisp)))
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
              "~&Warning: different version of Shelly was detected. Try \"shly install\".~%")
      (force-output *error-output*))
    (values)))

(defun load-shlyfile (shlyfile)
  (pushnew (directory-namestring (asdf::truenamize shlyfile))
           asdf:*central-registry*)
  (let ((*standard-output* (make-broadcast-stream))
        #+quicklisp (original-quickload #'ql:quickload))
    (flet (#+quicklisp
           (ql:quickload (systems &rest args)
            (apply #'original-quickload systems args)))
      (load shlyfile))))

@export
(defun load-local-shlyfile (&optional shlyfile)
  (when (and shlyfile
             (not (fad:file-exists-p shlyfile)))
    (format *error-output* "Error: No such shlyfile: \"~A\""
            shlyfile)
    (swank-backend:quit-lisp))

  (let ((shlyfile (or shlyfile
                      (car (member-if #'fad:file-exists-p
                                      '(#P"shlyfile" #P"shlyfile.lisp" #P"shlyfile.cl"))))))
    (when shlyfile
      (load-shlyfile shlyfile))))

@export
(defun load-global-shlyfile ()
  (let* ((shelly-home
          (merge-pathnames ".shelly/" (user-homedir-pathname)))
         (shlyfile
          (car (member-if #'fad:file-exists-p
                          (mapcar #'(lambda (path)
                                      (merge-pathnames path shelly-home))
                                  '(#P"shlyfile" #P "shlyfile.lisp" #P"shlyfile.cl"))))))
    (when shlyfile
      (load-shlyfile shlyfile))))

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
