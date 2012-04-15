#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly-asd
  (:use :cl :asdf))
(in-package :shelly-asd)

(defsystem shelly
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "BSD"
  :depends-on (:swank)
  :components ((:module "src"
                :components
                ((:file "shelly"))))
  :description ""
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
