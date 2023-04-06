;;; -*-Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER -*-
;;;
;;; Time-stamp: <2022-12-14 07:41:40 fred>
;;; Defsystem file for logo-to-lisp translator.
;;; Original: FMG 8-AUG-1999
;;;
(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  #+(and) (pushnew :graphics *features*)
  )

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note)))

(in-package :asdf)

#+sbcl (setf sb-ext:*on-package-variance* '(:warn (:logo) :error t))

(defsystem "logo"
  :version                 (:read-file-form "package.lisp" :at (1 2))
  :long-name               "Logo-to-Lisp Translator"
  :description             "Logo-to-lisp translator, compiler and runtime system."
  :long-description        "Compiles Logo programs to machine code by tranlating to lisp and compiling with the lisp compiler. 
Includes turtle graphics."
  :author                  "Fred Gilham <fmgilham@gmail.com>"
  :license                 "MIT"
  :depends-on              (#+graphics "clx" "logo/internals"))

(defsystem "logo/internals"
  :pathname ""
  :components
  ((:file "package")
   (:file "defs"            :depends-on ("package"))
   (:file "logo-procedure"  :depends-on ("package" "defs"))
   (:file "errors"          :depends-on ("package" "defs"))
   (:file "reader"          :depends-on ("package" "defs" "errors"))
   (:file "translator"      :depends-on ("package" "defs" "reader" "logo-procedure"))
   (:file "infix"           :depends-on ("package" "defs" "translator" "logo-procedure"))
   (:file "procedures"      :depends-on ("package" "defs" "logo-procedure" "translator"))
   #+graphics 
   (:file "turtle"          :depends-on ("package" "defs" "logo" "translator" "logo-procedure"))
   (:file "toplevel"        :depends-on ("package" "defs" "translator"))
   (:file "logo"            :depends-on ("package" "defs" "toplevel"))))


