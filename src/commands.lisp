#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.commands
  (:use :cl))
(in-package :shelly.commands)

(cl-annot:enable-annot-syntax)

@export
(defvar *shelly-commands-package* :shelly.commands)

@export
(defvar *shelly-args* '())
