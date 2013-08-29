#|
  This file is a part of shelly project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage shelly.versions
  (:use :cl)
  (:import-from :yason
                :parse)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :drakma
                :http-request)
  (:import-from :archive
                :open-archive
                :extract-files-from-archive
                :read-entry-from-archive)
  (:import-from :chipz
                :make-decompressing-stream
                :gzip))
(in-package :shelly.versions)

(cl-annot:enable-annot-syntax)

(let (releases)
  (defun retrieve-releases ()
    (labels ((retrieve-from-api ()
               (let ((res
                      (yason:parse
                       (flex:octets-to-string
                        (drakma:http-request "https://api.github.com/repos/fukamachi/shelly/tags")))))
                 res)))

      (or releases
          (setf releases (retrieve-from-api))))))

@export
(defun release-versions ()
  (let ((releases (retrieve-releases)))
    (mapcar #'(lambda (release) (gethash "name" release)) releases)))

@export
(defun find-version (version)
  (case version
    (:latest (find-version (car (sort (release-versions) #'string>))))
    ('nil nil)
    (t (car
        (member-if #'(lambda (release)
                       (string= (gethash "name" release)
                                version))
                   (retrieve-releases))))))

(defun version-tarball-url (version)
  (let ((version (find-version version)))
    (if version
        (gethash "tarball_url" version)
        nil)))

@export
(defun download-version (version &optional (destination *default-pathname-defaults*))
  (let ((tarball-url (version-tarball-url version)))
    (unless tarball-url
      (error "Version ~A is not found." version))

    (multiple-value-bind (tarball-stream status)
        (drakma:http-request tarball-url :want-stream t)
      (unless (= status 200)
        (error "Failed to download a tarball from GitHub (~A / status=~D)."
               tarball-url status))

      (unwind-protect
          (extract-tarball tarball-stream destination)
        (close tarball-stream)))))

(defun extract-tarball (tarball-stream &optional (destination *default-pathname-defaults*))
  (let ((*default-pathname-defaults* destination))
    (let ((archive (archive:open-archive 'archive:tar-archive
                    (chipz:make-decompressing-stream 'chipz:gzip tarball-stream)
                    :direction :input)))
      (prog1
        (merge-pathnames
         (archive:name (archive:read-entry-from-archive archive))
         *default-pathname-defaults*)
        (archive::extract-files-from-archive archive)))))
