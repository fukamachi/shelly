(in-package :cl-user)
(defpackage shelly.versions
  (:use :cl
        :split-sequence)
  (:export :release-versions
           :find-version
           :find-version-name
           :version<
           :version<=
           :download-version))
(in-package :shelly.versions)

(defun qlrequire (packages)
  (dolist (package packages)
    (unless (find-package package)
      (let ((*standard-output* (make-broadcast-stream)))
        #+quicklisp (ql:quickload package :verbose nil)
        #-quicklisp (asdf:load-system package :verbose nil)))))

(defmacro i (symbol &optional (args nil p))
  (let* ((pos (position #\# (symbol-name symbol)))
         (intern `(intern ,(subseq (symbol-name symbol) (1+ pos))
                          (find-package ,(subseq (symbol-name symbol) 0 pos)))))
    (if p
        `(funcall ,intern ,@args)
        intern)))

(let (releases)
  (defun retrieve-releases ()
    (qlrequire '(:yason :drakma :flexi-streams))
    (labels ((retrieve-from-api ()
               (let ((res
                       (i #:yason#parse
                          ((i #:flex#octets-to-string
                              ((i #:drakma#http-request ("https://api.github.com/repos/fukamachi/shelly/tags"))))))))
                 res)))
      (or releases
          (setf releases (retrieve-from-api))))))

(defun release-versions ()
  (let ((releases (retrieve-releases)))
    (mapcar #'(lambda (release) (gethash "name" release)) releases)))

(defun find-version (version)
  (when (string= version "latest")
    (return-from find-version (find-version :latest)))

  (case version
    (:latest (find-version (car (sort (release-versions) #'string>))))
    ('nil nil)
    (t (car
        (member-if #'(lambda (release)
                       (string= (gethash "name" release)
                                version))
                   (retrieve-releases))))))

(defun find-version-name (version)
  (let ((version (find-version version)))
    (when version
      (gethash "name" version))))

(defun version< (version1 version2)
  (and (not (string= version1 version2))
       (version<= version1 version2)))

(defun version<= (version1 version2)
  (flet ((digit-string<= (a b)
           (<= (parse-integer a) (parse-integer b))))
    (loop for a in (split-sequence #\. version1 :count 3)
          for b in (split-sequence #\. version2 :count 3)
          unless (digit-string<= a b)
            do (return-from version<= nil)
          finally
             (return t))))

(defun version-tarball-url (version)
  (let ((version (find-version version)))
    (if version
        (gethash "tarball_url" version)
        nil)))

(defun download-version (version &optional (destination *default-pathname-defaults*))
  (let ((tarball-url (version-tarball-url version)))
    (unless tarball-url
      (error "Version ~A is not found." version))
    (qlrequire '(:drakma :archive :chipz))

    (multiple-value-bind (tarball-stream status)
        (i #:drakma#http-request (tarball-url :want-stream t))
      (unless (= status 200)
        (error "Failed to download a tarball from GitHub (~A / status=~D)."
               tarball-url status))

      (unwind-protect
           (extract-tarball tarball-stream destination)
        (close tarball-stream)))))

(defun extract-tarball (tarball-stream &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (qlrequire '(:archive :chipz))
    (let ((archive (i #:archive#open-archive
                      ((i #:archive#tar-archive)
                       (i #:chipz#make-decompressing-stream ((i #:chipz#gzip) tarball-stream))
                       :direction :input))))
      (prog1
          (merge-pathnames
           (i #:archive#name ((i #:archive#read-entry-from-archive (archive))))
           *default-pathname-defaults*)
        (i #:archive#extract-files-from-archive (archive))))))
