#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.install
  (:use :cl)
  (:import-from :asdf
                :getenv)
  (:import-from :cl-fad
                :copy-file
                :file-exists-p
                :delete-directory-and-files)
  (:import-from :shelly.impl
                :*current-lisp-name*
                :*current-lisp-path*
                :*eval-option*)
  (:import-from :shelly.versions
                :download-version)
  (:import-from :shelly.util
                :shadowing-use-package
                :copy-directory
                :terminate))
(in-package :shelly.install)

(cl-annot:enable-annot-syntax)

@export
(defun install (&key version)
  "Install Shelly into your environment under \"~/.shelly\".
You can install a specific version by using \"--version\"."
  (let ((shelly-system-path
         (if version
             (download-version version
                               (merge-pathnames ".shelly/" (user-homedir-pathname)))
             (asdf:system-source-directory :shelly))))
    (install-from-path shelly-system-path)
    (when version
      (delete-directory-and-files shelly-system-path)))
  (values))

(defun install-from-path (shelly-system-path)
  (let* ((home-config-path
          (merge-pathnames ".shelly/" (user-homedir-pathname)))
         (shly-path
          (merge-pathnames "bin/shly" shelly-system-path))
         (version (let ((asdf:*central-registry*
                         (cons shelly-system-path asdf:*central-registry*)))
                    (slot-value (asdf:find-system :shelly)
                                'asdf:version))))

    (ensure-directories-exist home-config-path)

    (with-open-file (out (merge-pathnames
                          (format nil "config.~A" *current-lisp-name*)
                          home-config-path)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out "# -*- mode: perl -*-

{
~:[~;~:*    binary_path => \"~A\",~]
}
"
              (or
               (getenv "LISP_BINARY")
               *current-lisp-path*)))

    (with-open-file (out (merge-pathnames "config" home-config-path)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out "# -*- mode: perl -*-

{
    default_lisp => \"~A\",
    version => \"~A\",
    quicklisp_home => ~:[undef~;~:*\"~A\"~]
}
"
              *current-lisp-name*
              version
              #+quicklisp ql::*quicklisp-home* #-quicklisp nil))

    (dolist (dir '("bin/"))
      (ensure-directories-exist
       (merge-pathnames dir home-config-path)))

    (cond
      ((fad:file-exists-p shly-path)
       (fad:copy-file shly-path
        (merge-pathnames "bin/shly" home-config-path)
        :overwrite t)
       ;; XXX: must be more portable.
       (asdf:run-shell-command
        "chmod u+x ~A"
        (merge-pathnames "bin/shly" home-config-path)))
      (t
       (warn "Shelly script doesn't exist. Ignored.")))

    (let ((shelly-dir (merge-pathnames #P"shelly/" home-config-path)))
      (delete-directory-and-files shelly-dir
                                  :if-does-not-exist :ignore)
      (copy-directory shelly-system-path
                      shelly-dir)))

  (format t "~&
Successfully installed!
Add this to your shell rc file (\".bashrc\", \".zshrc\" and so on).

    PATH=$HOME/.shelly/bin:$PATH

"))
