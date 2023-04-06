;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: "LOGO" -*-
;;;
;;; Time-stamp: <2023-03-20 12:09:27 fred>
;;; Logo-to-lisp translator: package.lisp
;;; Define a couple of imortant things.
;;; Original: FMG 10-APR-2015

(in-package "COMMON-LISP-USER")

(defvar *logo-version* "1.0.1")

(uiop:define-package "LOGO"
  (:use :cl)
  (:shadow #:load #:print #:equalp
	   #:listp #:list #:first #:second #:last #:emptyp #:reverse
	   #:type
	   #:array #:count
	   #:random #:round #:log #:sqrt #:sin #:cos #:tan #:exp
	   #:pi
	   #:position
	   #:disassemble
	   )
  (:export #:logo-toplevel 
	   ;; Debugging
	   #:*debug* #:*debug-infix* #:*break-on-errors* #:*error-debug*
	   #:dump-procedures #:dump-primitives #:dump-operators
	   #:dump-commands #:dump-specials))
;;;
;;; XXXX User-defined symbols, procedures etc. should go in here.
;;;
(defpackage "LOGO-USER"
  (:use :logo)
  (:nicknames "LU"))


(in-package "LOGO")

;;; Development and production compiler settings.
(defparameter *compile-options*
  #+(and)
  '(optimize (speed 2) (safety 3) (debug 3) #+cmu (ext:inhibit-warnings 3) #+sbcl (sb-ext:inhibit-warnings 3))
  #-(and)
  '(optimize (speed 3) (safety 1) (debug 1) #+cmu (ext:inhibit-warnings 3) #+sbcl (sb-ext:inhibit-warnings 3)))
