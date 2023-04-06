;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER -*-
;;;
;;; Time-stamp: <2022-12-14 07:44:49 fred>
;;; Logo-to-lisp translator: logo.lisp
;;;
;;; Original: FMG 8-AUG-1999


(in-package "LOGO")

(declaim #.*compile-options*)

(defun logo ()
  (logo:logo-toplevel))

;;; Tired of typing (logo) and getting an error....
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'logo (find-package "COMMON-LISP-USER")))
