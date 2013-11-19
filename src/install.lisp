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
                :*eval-option*
                :save-core-image)
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

    ;; Delete dumped cores of all Lisp implementations
    ;; if the installing version is different from the current version.
    (let ((current-installed-version (getenv "SHELLY_VERSION")))
      (unless (or (not current-installed-version)
                  (string= version current-installed-version))
        (map nil #'delete-file
             (fad:list-directory (merge-pathnames "dumped-cores/" home-config-path)))))

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

    (dolist (dir '("dumped-cores/" "bin/"))
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
                      shelly-dir))

    (dump-core :quit-lisp nil))

  (format t "~&
Successfully installed!
Add this to your shell rc file (\".bashrc\", \".zshrc\" and so on).

    PATH=$HOME/.shelly/bin:$PATH

"))

(defvar *dumped-core-path*
    (merge-pathnames (format nil "dumped-cores/~A.core"
                             *current-lisp-name*)
                     (merge-pathnames ".shelly/" (user-homedir-pathname))))

@export
(defun dump-core (&key (quit-lisp t) (path *dumped-core-path*))
  "Dump Lisp core image file for faster startup.
You can designate which path to dump by using \"--path\"."
  (cond
    (quit-lisp
     #+quicklisp (ql:quickload :shelly)
     #-quicklisp (asdf:load-system :shelly)
     (shelly.util:shadowing-use-package :shelly)
     (save-core-image path))
    (T
     (asdf:run-shell-command "~A ~A ~A '~S' ~A '~S' ~A '~A' ~A '~A' ~A '~S'"
      *current-lisp-path*

      #+ccl "--no-init"
      #+sbcl "--no-userinit"
      #+allegro "-qq"
      #+clisp "-norc"
      #+cmu "-noinit"
      #+ecl "-norc"
      #-(or ccl sbcl allegro clisp cmu ecl) ""

      *eval-option*
      #+quicklisp
      (let ((quicklisp-init (merge-pathnames #P"setup.lisp" ql:*quicklisp-home*)))
        (if (probe-file quicklisp-init)
            `(load ,quicklisp-init)
            ""))
      #-quicklisp
      '(require (quote asdf))

      *eval-option*
      `(push ,(asdf:system-source-directory :shelly) asdf:*central-registry*)

      *eval-option*
      "(let (#+allegro(*readtable* (copy-readtable))) #+quicklisp (ql:quickload :shelly) #-quicklisp (asdf:load-system :shelly))"
      *eval-option*
      "(shelly.util:shadowing-use-package :shelly)"
      *eval-option*
      '(shelly.impl:save-core-image path)))))

@export
(defun rm-core ()
  "Remove saved core image file which created by `dump-core'."
  (handler-case
      (progn (delete-file *dumped-core-path*)
             (format t "~&Successfully deleted: ~A~%"
                     *dumped-core-path*))
    (file-error (c) (princ c)))

  (terminate))
