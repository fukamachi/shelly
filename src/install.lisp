(in-package :cl-user)
(defpackage shelly.install
  (:use :cl)
  (:import-from :asdf
                :getenv)
  (:import-from :cl-fad
                :copy-file
                :file-exists-p
                :delete-directory-and-files
                :walk-directory)
  (:import-from :local-time
                :format-timestring
                :now)
  (:import-from :shelly.impl
                :*current-lisp-name*
                :*current-lisp-path*
                :command-line-args
                :*eval-option*
                :save-core-image
                :save-app)
  (:import-from :shelly.versions
                :download-version
                :find-version)
  (:import-from :shelly.util
                :shelly-home
                :shadowing-use-package
                :copy-directory
                :print-package-commands
                :arglist
                :terminate))
(in-package :shelly.install)

(cl-annot:enable-annot-syntax)

@export
(defun install (&key version)
  "Install Shelly into your environment under \"~/.shelly\".
You can install a specific version by using \"--version\"."
  (let ((shelly-system-path
         (if version
             (download-version version (shelly-home))
             (asdf:system-source-directory :shelly))))
    (when (equal shelly-system-path
                 (merge-pathnames #P"shelly/" (shelly-home)))
      (terminate 1 "~&You already have this version. Exit.~%"))
    (when version
      (push shelly-system-path asdf:*central-registry*)
      #+quicklisp (ql:quickload :shelly)
      #-quicklisp (asdf:load-system :shelly))
    (install-from-path shelly-system-path)
    (when version
      (delete-directory-and-files shelly-system-path)))
  (values))

@export
(defun upgrade ()
  "Upgrade Shelly to the latest version."
  (let ((current-version (format nil "v~A"
                                 (slot-value (asdf:find-system :shelly)
                                             'asdf:version)))
        (latest-version (gethash "name" (find-version :latest))))
    (cond
      ((string< current-version latest-version)
       (format t "~&Upgrading Shelly from ~A to ~A.~%"
               current-version
               latest-version)
       (install :version latest-version))
      (T
       (format t "~&You already have the latest version.~%")))
    (values)))

(defun csh-style-init ()
  (format nil "set SHELLY_HOME = ~A
if ( -e $SHELLY_HOME/shelly/init.csh ) source $SHELLY_HOME/shelly/init.csh"
          (shelly-home)))

(defun sh-style-init ()
  (format nil
          "SHELLY_HOME=~A; [ -s \"$SHELLY_HOME/shelly/init.sh\" ] && . \"$SHELLY_HOME/shelly/init.sh\""
          (shelly-home)))

(defun configure-shell ()
  (labels ((rcfile (shell)
             (let ((program (file-namestring shell)))
               (cond
                 ((string= program "csh")  (values ".cshrc"   (csh-style-init)))
                 ((string= program "tcsh") (values ".tcshrc"  (csh-style-init)))
                 ((string= program "bash") (values ".bashrc"  (sh-style-init)))
                 ((string= program "zsh")  (values ".zshrc"   (sh-style-init)))
                 ((string= program "sh")   (values ".profile" (sh-style-init))))))
           (slurp-stream (stream)
             (let ((seq (make-string (file-length stream))))
               (read-sequence seq stream)
               seq))
           (slurp-file (file)
             (with-open-file (input file)
               (slurp-stream input))))
    (multiple-value-bind (rcfile rc) (rcfile (getenv "SHELL"))
      (when rcfile
        (setf rcfile (merge-pathnames rcfile (user-homedir-pathname))))
      (when (and rcfile
                 rc
                 (fad:file-exists-p rcfile))
        (cond
          ((search rc (slurp-file rcfile))
           (format t "~2&Your shell has already configured in ~A. Enjoy!~%"
                   rcfile))
          (T
           (format t "~2&Adding the following settings into your ~A:~2%    ~A~2%"
                   rcfile rc)
           (with-open-file (output rcfile :direction :output :if-exists :append)
             (fresh-line output)
             (format output rc)
             (fresh-line output))
           (format t "~&The configuration succeeded. Please reload your ~A. Enjoy!~%"
                   rcfile)))))))

(defun install-from-path (shelly-system-path)
  (let* ((home-config-path (shelly-home))
         (version (slot-value (asdf:find-system :shelly)
                              'asdf:version)))

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
      (format out "SHELLY_LISP_IMPL=\"~A\"~%SHELLY_LISP_BINARY=\"~A\"~%SHELLY_VERSION=\"~A\"~%QUICKLISP_HOME=~:[~;~:*\"~A\"~]~%"
              *current-lisp-name*
              *current-lisp-path*
              version
              #+quicklisp ql:*quicklisp-home*
              #-quicklisp nil))

    (dolist (dir '("dumped-cores/" "bin/"))
      (ensure-directories-exist
       (merge-pathnames dir home-config-path)))

    (let ((bin-dir (merge-pathnames #P"bin/" home-config-path)))
      (delete-directory-and-files bin-dir
                                  :if-does-not-exist :ignore)
      (copy-directory (merge-pathnames #P"bin/" shelly-system-path)
                      bin-dir)
      (fad:walk-directory bin-dir
                          (lambda (file)
                            ;; XXX: must be more portable.
                            (asdf:run-shell-command "chmod u+x ~A" file))))

    (let ((shelly-dir (merge-pathnames #P"shelly/" home-config-path)))
      (delete-directory-and-files shelly-dir
                                  :if-does-not-exist :ignore)
      (copy-directory shelly-system-path
                      shelly-dir))
    (dump-core :quit-lisp nil))

  (format t "~&Successfully installed!~%")

  (configure-shell))

(defun dumped-core-path ()
  (merge-pathnames (format nil "dumped-cores/~A.core"
                           (getenv "LISP_IMPL"))
                   (shelly-home)))

@export
(defun dump-core (&key (quit-lisp t) load-systems (output (dumped-core-path)))
  "Dump Lisp core image file for faster startup."
  (declare (ignorable load-systems output))
  #-(or sbcl allegro ccl clisp cmu)
  (when quit-lisp
    (format *error-output* "~&~A does not support 'dump-core'.~%"
            *current-lisp-name*))
  #+(or sbcl allegro ccl clisp cmu)
  (asdf:run-shell-command "~A ~A ~A '~S' ~A '~S' ~A '~A' ~A '~S' ~A '~S'"
                          *current-lisp-path*

                          #+ccl "--no-init --quiet --batch"
                          #+sbcl "--noinform --no-sysinit --no-userinit --non-interactive"
                          #+allegro "--qq"
                          #+clisp "-norc --quiet --silent -on-error exit"
                          #+cmu "-noinit"

                          *eval-option*
                          #+quicklisp
                          (let ((quicklisp-init (merge-pathnames #P"setup.lisp" ql:*quicklisp-home*)))
                            (if (probe-file quicklisp-init)
                                `(load ,quicklisp-init)
                                ""))
                          #-quicklisp
                          '(require (quote asdf))

                          *eval-option*
                          `(push ,(merge-pathnames "shelly/" (shelly-home)) asdf:*central-registry*)

                          *eval-option*
                          (format nil
                                  "(let ((*standard-output* (make-broadcast-stream)) #+allegro(*readtable* (copy-readtable))) (mapc #+quicklisp (function ql:quickload) #-quicklisp (function asdf:load-system) (list ~{:~A~^ ~})))"
                                  (cons :shelly
                                        load-systems))

                          *eval-option*
                          `(shelly.util:shadowing-use-package :shelly)

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

#+(or sbcl ccl)
@export
(defmacro install-command (package-or-function-name)
  "Make an executable file under SHELLY_HOME/bin/.

Example:
    $ shly -Lclack install-command clack:clackup
    $ shly -Lclack install-command clack.app.directory

    $ clackup /path/to/app.lisp
    $ clack.app.directory start-server --port 50032
"
  `(destructuring-bind (package-name &optional function-name)
       (if (symbolp ',package-or-function-name)
           (list
            (package-name (symbol-package ',package-or-function-name))
            (symbol-name ',package-or-function-name))
           (ppcre:split ":+" ',package-or-function-name :limit 2))
     (unless (find-package (string-upcase package-name))
       (error "Package ~S does not exist." package-name))

     (if function-name
         (install-function-command package-name function-name)
         (install-package-command package-name))))

#+(or sbcl ccl)
(defun install-function-command (package-name function-name)
  (let ((fn (intern (string-upcase function-name) (string-upcase package-name))))
    (unless (fboundp fn)
      (error "Function ~S does not exist." fn))
    (let* ((arglist (arglist fn))
           (help-message
             (format nil "Usage:~%    $ ~~A ~:[()~;~:*~(~A~)~]~2%~{~&    ~A~^~%~}~%"
                     (if (eq arglist :not-available)
                         ""
                         arglist)
                     (ppcre:split "\\n" (documentation fn 'function))))
           (version-message
             (format nil "~&~(~A~) in ~(~A~)~:[~;~:* ~A~]~%Generated by Shelly ~A with ~A ~A at ~A~%"
                     function-name package-name
                     (and (asdf:find-system (string-downcase package-name) nil)
                          (asdf:component-version (asdf:find-system (string-downcase package-name))))
                     (asdf:component-version (asdf:find-system :shelly))
                     (lisp-implementation-type)
                     (lisp-implementation-version)
                     (format-timestring nil (now)
                                        :format '((:year 4) #\/ (:month 2) #\/ (:day 2)))))
           (bin-path (merge-pathnames function-name (merge-pathnames #P"bin/" (shelly-home)))))
      (flet ((main ()
               (cond
                 ((null (cdr (command-line-args)))
                  (format t help-message
                          (file-namestring (car (command-line-args)))))
                 ((string= (second (command-line-args)) "--version")
                  (princ version-message))
                 (T
                  (shelly.core::interpret (cons (princ-to-string fn)
                                                (cdr (command-line-args))))))))
        (save-app bin-path #'main)))))

#+(or sbcl ccl)
(defun install-package-command (package-name)
  (let ((help-message
          (with-output-to-string (s)
            (format s "Usage:~%    $ ~~A [command] [arg1,arg2...]~2%Commands:~%")
            (print-package-commands (string-upcase package-name) s)))
        (version-message
          (format nil "~&~A~:[~;~:* ~A~]~%Generated by Shelly ~A with ~A ~A at ~A~%"
                  package-name (and (asdf:find-system package-name)
                                    (asdf:component-version (asdf:find-system package-name)))
                  (asdf:component-version (asdf:find-system :shelly))
                  (lisp-implementation-type)
                  (lisp-implementation-version)
                  (format-timestring nil (now)
                                     :format '((:year 4) #\/ (:month 2) #\/ (:day 2)))))
        (bin-path (merge-pathnames package-name (merge-pathnames #P"bin/" (shelly-home)))))
    (flet ((main ()
             (cond
               ((null (cdr (command-line-args)))
                (format t help-message
                        (file-namestring (car (command-line-args)))))
               ((string= (second (command-line-args)) "--version")
                (princ version-message))
               (T
                (destructuring-bind (command &rest args) (cdr (command-line-args))
                  (shelly.core::interpret (cons (format nil "~A:~A" package-name command)
                                                args)))))))
      (save-app bin-path #'main))))
