(in-package :cl-user)
(defpackage shelly
  (:use :cl
        :split-sequence)
  (:import-from :shelly.core
                :run-repl
                :*argv*)
  (:import-from :shelly.install
                :install
                :upgrade
                :uninstall
                :dump-core
                :local-dump-core
                :rm-core
                :install-command)
  (:import-from :shelly.versions
                :release-versions)
  (:import-from :shelly.util
                :local-command-symbols
                :arglist
                :print-package-commands)
  (:export :*argv*
           :run-repl
           :install
           :upgrade
           :uninstall
           :dump-core
           :local-dump-core
           :rm-core
           :install-command

           :help
           :available-versions))
(in-package :shelly)

(defun help (&optional command)
  "Show a list of Built-In Commands.
If `command' is specified, its usage will be displayed."
  (if command
      (let* ((parsed-command (let ((*package* (find-package :cl-user)))
                               (read-from-string command)))
             (arglist (arglist parsed-command)))
        (format t "~&Usage: ~A ~:[()~;~:*~(~A~)~]~{~&    ~A~}~%"
                command
                (if (eq arglist :not-available)
                    ""
                    arglist)
                (split-sequence #\Newline (documentation parsed-command 'function))))
      (progn
        (format t "~&Built-In Commands:~%")
        (print-package-commands :shelly)
        (let ((symbols (local-command-symbols)))
          (when symbols
            (format t "~&Local Commands:~%")
            (dolist (symbol symbols)
              (let ((arglist (arglist symbol)))
                (format t "~&    ~(~A~) ~:[()~;~:*~(~A~)~]~{~&        ~A~}~2%"
                        symbol
                        (if (eq arglist :not-available)
                            ""
                            arglist)
                        (split-sequence #\Newline (documentation symbol 'function)))))))))
  (values))

(defun available-versions ()
  "Show all the possible Shelly versions."
  (format t "~{~&~A~%~}" (release-versions))
  (values))
