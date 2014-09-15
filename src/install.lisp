(in-package :cl-user)
(defpackage shelly.install
  (:use :cl
        :split-sequence)
  (:import-from :asdf
                :getenv)
  (:import-from :cl-fad
                :copy-file
                :file-exists-p
                :directory-exists-p
                :delete-directory-and-files
                :list-directory)
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
                :find-version-name
                :version<
                :version<=)
  (:import-from :shelly.util
                :shelly-home
                :shadowing-use-package
                :copy-directory
                :print-package-commands
                :arglist
                :terminate)
  (:export :install
           :upgrade
           :uninstall
           :dump-core
           :local-dump-core
           :rm-core
           :install-command))
(in-package :shelly.install)

(defun install (&key version global (directory (shelly-home) directory-specified-p))
  "Install Shelly into your environment under \"~/.shelly\".
You can install a specific version by using \"--version\"."
  (when global
    (when directory-specified-p
      (error "Both of --global and --directory cannot be specified at the same time."))
    (setf directory #P"/usr/local/"))
  (setf directory (fad:pathname-as-directory directory))
  (let ((shelly-system-path
         (if version
             (download-version version (merge-pathnames #P"lib/" directory))
             (asdf:system-source-directory :shelly))))
    (when (equal shelly-system-path
                 (merge-pathnames #P"lib/shelly/" directory))
      (terminate 1 "~&You already have this version. Exit.~%"))
    (when version
      (push shelly-system-path asdf:*central-registry*)
      #+quicklisp (ql:quickload :shelly)
      #-quicklisp (asdf:load-system :shelly))
    (if (version<= "0.8.1"
                   (if version
                       (subseq (find-version-name version) 1)
                       (asdf:component-version (asdf:find-system :shelly))))
        (install-from-path shelly-system-path directory)
        (install-from-path shelly-system-path))
    (when version
      (delete-directory-and-files shelly-system-path)))
  (values))

(defun upgrade ()
  "Upgrade Shelly to the latest version."
  (let ((current-version (slot-value (asdf:find-system :shelly)
                                     'asdf:version))
        (latest-version (subseq (find-version-name :latest) 1)))
    (cond
      ((version< current-version latest-version)
       (format t "~&Upgrading Shelly from ~A to ~A.~%"
               current-version
               latest-version)
       (install :version (format nil "v~A" latest-version)
                :directory (truename (asdf:system-relative-pathname :shelly #P"../../"))))
      (T
       (format t "~&You already have the latest version.~%")))
    (values)))

(defun uninstall (&key (directory (truename (asdf:system-relative-pathname :shelly #P"../../"))))
  "Uninstall Shelly."
  (when (getenv "SHELLY_PATH")
    (error "Cannot uninstall when SHELLY_PATH is specified."))
  (uninstall-with-path directory))

(defun uninstall-with-path (install-dir)
  (let ((shelly-dir (merge-pathnames #P"lib/shelly/" install-dir)))
    (unless (fad:directory-exists-p shelly-dir)
      (error "~S does not exist." shelly-dir))
    (let ((shly-bin (merge-pathnames #P"bin/shly" install-dir)))
      (when (and (probe-file shly-bin)
                 (string= (directory-namestring (truename shly-bin))
                          (directory-namestring (merge-pathnames #P"bin/" shelly-dir))))
        (delete-file shly-bin)))
    (delete-directory-and-files shelly-dir)))

(defun csh-style-init (install-dir)
  (let ((shelly-home (fad:pathname-as-file (shelly-home))))
    (format nil "set SHELLY_HOME = ~A
if ( -e ~Ashelly/init.csh ) source ~:*~Alib/shelly/init.csh"
            shelly-home
            (if (equal shelly-home (fad:pathname-as-file install-dir))
                "$SHELLY_HOME/"
                install-dir))))

(defun sh-style-init (install-dir)
  (let ((shelly-home (fad:pathname-as-file (shelly-home))))
    (format nil
            "SHELLY_HOME=~A; [ -s \"~Alib/shelly/init.sh\" ] && . \"~:*~Alib/shelly/init.sh\""
            shelly-home
            (if (equal shelly-home (fad:pathname-as-file install-dir))
                "$SHELLY_HOME/"
                install-dir))))

(defun configure-shell (install-dir)
  (labels ((rcfile (shell)
             (let ((program (file-namestring shell)))
               (cond
                 ((string= program "csh")  (values ".cshrc"   (csh-style-init install-dir)))
                 ((string= program "tcsh") (values ".tcshrc"  (csh-style-init install-dir)))
                 ((string= program "bash") (values ".bashrc"  (sh-style-init install-dir)))
                 ((string= program "zsh")  (values ".zshrc"   (sh-style-init install-dir)))
                 ((string= program "sh")   (values ".profile" (sh-style-init install-dir))))))
           (slurp-stream (stream)
             (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
               (read-sequence seq stream)
               seq))
           (slurp-file (file)
             (with-open-file (input file :element-type '(unsigned-byte 8))
               (slurp-stream input))))
    (multiple-value-bind (rcfile rc) (rcfile (getenv "SHELL"))
      (when rcfile
        (setf rcfile (merge-pathnames rcfile (user-homedir-pathname))))
      (when (and rcfile
                 rc
                 (fad:file-exists-p rcfile))
        (cond
          ((search (babel:string-to-octets rc) (slurp-file rcfile))
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

(defun install-from-path (shelly-system-path &optional (install-dir (shelly-home)))
  (ensure-directories-exist install-dir)
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
              #-win32
              *current-lisp-path*
              #+win32
              (substitute #\/ #\\ *current-lisp-path*)

              version
              #+quicklisp ql:*quicklisp-home*
              #-quicklisp nil))

    (dolist (home-dir '("dumped-cores/" "bin/"))
      (ensure-directories-exist
       (merge-pathnames home-dir home-config-path)))

    (let ((home-bin-dir (merge-pathnames #P"bin/" home-config-path)))
      (ensure-directories-exist home-bin-dir)
      (when (probe-file (merge-pathnames #P"shly" home-bin-dir))
        (delete-file (merge-pathnames #P"shly" home-bin-dir))))

    (delete-directory-and-files (merge-pathnames #P"shelly/" install-dir)
                                :if-does-not-exist :ignore)
    (let ((shelly-dir (merge-pathnames #P"lib/shelly/" install-dir)))
      (when (fad:directory-exists-p shelly-dir)
        (uninstall-with-path install-dir))
      (copy-directory shelly-system-path
                      shelly-dir)

      (push shelly-dir asdf:*central-registry*)
      #+quicklisp (ql:quickload :shelly)
      #-quicklisp (asdf:load-system :shelly))

    (let ((install-bin-dir (merge-pathnames #P"bin/" install-dir))
          (shly-bin (merge-pathnames #P"lib/shelly/bin/shly" install-dir)))
      (ensure-directories-exist install-bin-dir)
      ;; XXX: must be more portable.
      (uiop:run-program (list "chmod" "u+x" (princ-to-string shly-bin)))
      (uiop:run-program (list "ln" "-s"
                              (princ-to-string shly-bin)
                              (princ-to-string install-bin-dir))))

    (dump-core :quit-lisp nil))

  (format t "~&Successfully installed!~%")

  (configure-shell install-dir))

(defun dumped-core-path ()
  (merge-pathnames (format nil "dumped-cores/~A.core"
                           (getenv "LISP_IMPL"))
                   (shelly-home)))

(defun dump-core (&key (quit-lisp t) load-systems (output (dumped-core-path)))
  "Dump Lisp core image file for faster startup."
  (declare (ignorable load-systems output))
  #-(or sbcl allegro ccl clisp cmu)
  (when quit-lisp
    (format *error-output* "~&~A does not support 'dump-core'.~%"
            *current-lisp-name*))

  #+(or sbcl allegro ccl clisp cmu)
  (uiop:run-program `(,*current-lisp-path*
                      #+sbcl
                      ,@(list "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
                      #+ccl
                      ,@(list "--no-init" "--quiet" "--batch")
                      #+allegro
                      "--qq"
                      #+clisp
                      ,@(list "-norc" "--quiet" "--silent" "-on-error" "exit")
                      #+cmu
                      "-noinit"

                      ,*eval-option*
                      ,(prin1-to-string
                        #+quicklisp
                        (let ((quicklisp-init (merge-pathnames #P"setup.lisp" ql:*quicklisp-home*)))
                          (if (probe-file quicklisp-init)
                              `(load ,quicklisp-init)
                              ""))
                        #-quicklisp
                        '(require (quote asdf)))

                      ,*eval-option*
                      ,(prin1-to-string `(push ,(asdf:system-source-directory :shelly) asdf:*central-registry*))

                      ,*eval-option*
                      ,(format nil
                               "(let ((*standard-output* (make-broadcast-stream)) #+allegro(*readtable* (copy-readtable))) (mapc #+quicklisp (function ql:quickload) #-quicklisp (function asdf:load-system) (list ~{:~A~^ ~})))"
                               (cons :shelly
                                     load-systems))

                      ,*eval-option*
                      ,(prin1-to-string `(shelly.util:shadowing-use-package :shelly))

                      ,*eval-option*
                      ,(prin1-to-string `(shelly.impl:save-core-image ,(princ-to-string output)))))

  (when quit-lisp
    (terminate))
  (values))

(defun local-dump-core (&rest systems)
  "(Experimental)
Dump Lisp core image file to the current directory.
This command takes system names to be included in the core."
  (ensure-directories-exist "dumped-cores/")
  (dump-core :quit-lisp nil
             :load-systems systems
             :output (format nil "dumped-cores/~A.core"
                             (getenv "LISP_IMPL"))))

(defun rm-core ()
  "Remove saved core image file which created by `dump-core'."
  (let ((path (dumped-core-path)))
    (handler-case
        (progn (delete-file path)
               (format t "~&Successfully deleted: ~A~%" path))
      (file-error (c) (princ c))))

  (terminate))

#+(or sbcl ccl clisp)
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
           (split-sequence #\: ',package-or-function-name :count 2 :remove-empty-subseqs t))
     (unless (find-package (string-upcase package-name))
       (error "Package ~S does not exist." package-name))

     (if function-name
         (install-function-command package-name function-name)
         (install-package-command package-name))))

#+(or sbcl ccl clisp)
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
                     (and (documentation fn 'function)
                          (split-sequence #\Newline (documentation fn 'function)))))
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

#+(or sbcl ccl clisp)
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
