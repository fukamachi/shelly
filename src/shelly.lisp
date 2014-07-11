#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly
  (:use :cl)
  (:import-from :cl-ppcre
                :split)
  (:import-from :shelly.core
                :run-repl)
  (:import-from :shelly.install
                :install)
  (:import-from :shelly.versions
                :release-versions)
  (:export :run-repl
           :install))
(in-package :shelly)

(cl-annot:enable-annot-syntax)

(defun help ()
  "Show a list of Built-In Commands."
  (format t "~&Built-In Commands:~%")
  (do-external-symbols (symbol :shelly)
    (format t "~&    ~(~A~)~%~{        ~A~^~%~}~2%"
            symbol
            (ppcre:split "\\n" (documentation symbol 'function))))
  (let (symbols (cl-user-package (find-package :cl-user)))
    (do-symbols (symbol cl-user-package)
      (when (and (eq cl-user-package (symbol-package symbol)) (fboundp symbol))
        (push symbol symbols)))
    (when symbols
      (format t "~&Local Commands:~%")
      (dolist (symbol symbols)
        (format t "~&    ~(~A~)~:[~;~:*~%~{        ~A~^~%~}~]~2%"
                symbol
                (ppcre:split "\\n" (documentation symbol 'function))))))
  (values))

@export
(defun available-versions ()
  "Show all the possible Shelly versions."
  (format t "~{~&~A~%~}" (release-versions))
  (values))
