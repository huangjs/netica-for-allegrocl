;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(defpackage :netica.sys
  (:use :cl :excl))

(in-package :netica.sys)

;;;
(defparameter *netica-sys-path* *load-truename*)

(defvar *netica-lib-path*
  #+(and linux x86-64) (merge-pathnames "lib/linux64/libnetica.so" *netica-sys-path*)
  #+(and linux x86) (merge-pathnames "lib/linux32/libnetica.so" *netica-sys-path*)
  #+(and mswindows x86-64) (merge-pathnames "lib/win64/Netica.dll" *netica-sys-path*)
  #+(and mswindows x86) (merge-pathnames "lib/win32/Netica.dll" *netica-sys-path*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load *netica-lib-path*))

(defsystem :netica (:default-pathname #.(merge-pathnames "src/" *netica-sys-path*))
  (:serial "netica.ffi.cl"
           "netica.ffi-fix.cl"
           "netica.cl"))

