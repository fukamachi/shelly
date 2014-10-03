#|
  This file is a part of shelly project.
  Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Shelly - Run Common Lisp from shell easily.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly-asd
  (:use :cl :asdf))
(in-package :shelly-asd)

(defsystem shelly
  :version "0.8.5"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:split-sequence
               :cl-fad
               :bordeaux-threads
               :local-time
               :trivial-signal
               :babel
               :uiop)
  :components ((:module "src"
                :components
                ((:file "shelly" :depends-on ("core" "install" "versions" "util"))
                 (:file "core" :depends-on ("impl" "error" "util"))
                 (:file "install" :depends-on ("core" "impl" "versions" "util"))
                 (:file "versions")
                 (:file "util" :depends-on ("error" "impl"))
                 (:file "impl")
                 (:file "error"))))
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
  :in-order-to ((test-op (test-op shelly-test))))
