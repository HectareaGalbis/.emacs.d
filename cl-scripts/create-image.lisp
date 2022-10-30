#!/usr/bin/env -S sbcl --script


;; Setup quicklisp
(let ((quicklisp-path (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-path)
    (load quicklisp-path)))


;; Get the command-line arguments
(defvar *script-file-name* (format nil "~a.~a"
				   (pathname-name (uiop:load-pathname))
				   (pathname-type (uiop:load-pathname))))
(defvar *arguments* (uiop:command-line-arguments))

(unless (>= (length *arguments*) 1)
  (format t "USAGE: ./~a IMAGE-FILE REQUIRED-SYSTEMS*~%" *script-file-name*)
  (quit))

(when (member (car *arguments*) '("-h" "--help") :test #'string=)
  (format t "USAGE: ./~a.~a IMAGE-FILE REQUIRED-SYSTEMS*~%~%" (pathname-name (uiop:load-pathname)) (pathname-type (uiop:load-pathname)))
  (format t "This script creates the file IMAGE-FILE containing a Lisp image. This image will have loaded the REQUIRED-SYSTEMS specified. Each system must be already installed and must be accessible to ASDF.~%~%")
  (format t "Example to create an image containing alexandria and cl-ppcre systems:~%  ./~a new_core.image alexandria cl-ppcre~%~%"
	  *script-file-name*)
  (quit))

(defvar *image-file* (car *arguments*))
(defvar *required-systems* (cdr *arguments*))

(unless (uiop:file-pathname-p *image-file*)
  (format t "ERROR: ~s is not a valid file name.~%" *image-file*)
  (quit))


;; Install and load required systems
(let ((systems (cdr *arguments*)))
  (loop for system in systems
	unless (asdf:find-system system nil)
	  do (format t "ERROR: ~s is not an available ASDF system.~%" system)
	     (quit)
	do (asdf:load-system system)))


;; Create the image
(uiop:dump-image (merge-pathnames (pathname *image-file*) (uiop:getcwd)))


