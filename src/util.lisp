#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.util
  (:use :cl))
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
  (ql:quickload systems :verbose nil :prompt nil)
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
