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

    (with-open-file (out (merge-pathnames "config" home-config-path)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out "# -*- mode: perl -*-

{
    version => \"~A\",
}
"
              version))

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

(defun dumped-core-path ()
  (merge-pathnames (format nil "dumped-cores/~A.core"
                           (getenv "LISP_IMPL"))
                   (merge-pathnames ".shelly/" (user-homedir-pathname))))

@export
(defun dump-core (&key (quit-lisp t) load-systems (output (dumped-core-path)))
  "Dump Lisp core image file for faster startup."
  (asdf:run-shell-command "~A ~A ~A '~S' ~A '~S' ~A '~A' ~A '~S'"
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
                          `(push ,(merge-pathnames ".shelly/shelly/" (user-homedir-pathname)) asdf:*central-registry*)

                          *eval-option*
                          (format nil
                                  "(let (#+allegro(*readtable* (copy-readtable))) (mapc #+quicklisp (function ql:quickload) #-quicklisp (function asdf:load-system) (list ~{:~A~^ ~})))"
                                  (cons :shelly
                                        load-systems))

                          *eval-option*
                          `(shelly.impl:save-core-image ,(princ-to-string output)))
  (when quit-lisp
    (terminate))
  (values))

@export
(defun local-dump-core (&rest systems)
  "(Experimental)
Dump Lisp core image file to the current directory.
This command takes system names to be included in the core."
  (ensure-directories-exist "dumped-cores/")
  (dump-core :quit-lisp nil
             :load-systems systems
             :output (format nil "dumped-cores/~A.core"
                             (getenv "LISP_IMPL"))))

@export
(defun rm-core ()
  "Remove saved core image file which created by `dump-core'."
  (let ((path (dumped-core-path)))
    (handler-case
        (progn (delete-file path)
               (format t "~&Successfully deleted: ~A~%" path))
      (file-error (c) (princ c))))

  (terminate))
