#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Shelly - Run Common Lisp from shell easily.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly-asd
  (:use :cl :asdf))
(in-package :shelly-asd)

(defsystem shelly
  :version "0.4.1"
  :author "Eitarow Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:cl-annot
               :swank
               :cl-fad
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "shelly" :depends-on ("core" "install"))
                 (:file "core" :depends-on ("impl"))
                 (:file "install" :depends-on ("impl" "util"))
                 (:file "util")
                 (:file "impl"))))
  :description "Run Common Lisp from shell easily."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op shelly-test))))

(defmethod asdf:perform :after ((op load-op) (c (eql (find-system :shelly))))
  (pushnew :shelly *features*))
