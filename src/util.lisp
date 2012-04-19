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
    (unless (string= version (slot-value (asdf:find-system :shelly)
                                         'asdf:version))
      (format *error-output*
              "Warning: different version of Shelly was detected. Try \\\"shly --install\\\".~%")
      (force-output *error-output*))
    (values)))
