#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly
  (:use :cl)
  (:import-from :shelly.core
                :run-repl)
  (:import-from :shelly.install
                :install
                :dump-core
                :rm-core)
  (:export :run-repl
           :install
           :dump-core
           :rm-core))
(in-package :shelly)
