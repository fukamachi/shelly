#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.impl
  (:use :cl))
(in-package :shelly.impl)

(cl-annot:enable-annot-syntax)

@export
(defvar *current-lisp-name*
    (or
     #+ccl "ccl"
     #+sbcl "sbcl"
     #+allegro "alisp"
     #+clisp "clisp"
     #+cmu "cmucl"
     #+ecl "ecl"))

@export
(defvar *current-lisp-path*
    (or
     #+ccl (car ccl:*command-line-argument-list*)
     #+sbcl (car sb-ext:*posix-argv*)
     #+allegro (car (system:command-line-arguments))
     #+clisp "clisp"
     #+cmu (car ext:*command-line-strings*)
     #+ecl (car (si:command-args))))

@export
(defvar *eval-option*
    (or
     #+ccl "--eval"
     #+sbcl "--eval"
     #+allegro "-e"
     #+clisp "-x"
     #+cmu "-eval"
     #+ecl "-eval"))

@export
(defun condition-undefined-function-name (condition)
  (or
   #+sbcl (slot-value condition 'sb-kernel::name)
   #+ecl (slot-value condition 'si::name)
   #+cmu (getf (conditions::condition-actual-initargs condition) :name)
   #+allegro (slot-value condition 'excl::name)
   #+ccl (slot-value condition 'ccl::name)
   #+clisp (slot-value condition 'system::$name)))
