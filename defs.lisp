;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-23 13:50:59 fred>
;;; Logo-to-lisp translator: defs.lisp
;;; Defvars, defparameters
;;; Original: FMG 10-APR-2015
;;;

(in-package "LOGO")

(declaim #.*compile-options*)

;;; Maps from "logo" names to procedure structures.
(defvar *procedure-names* (make-hash-table :test #'equal))
;;; Maps from function symbols to procedure structures.
(defvar *procedure-symbols* (make-hash-table :test #'eql))


(declaim (stream *current-stream*))
(defvar *current-stream* *query-io*)
(defvar *interactive* t) ; If t, then prompt.
(defvar *prompt-char* #\?)
(defvar *prompt*)

;;;
;;; Readtable hackery. See reader.lisp
;;;
(defvar *logo-readtable* (copy-readtable nil))

(defparameter *whitespace-bag* '(#\space #\tab #\newline #\return))

;; This gets set when reading a list to kludge in continuation
;; characters so you can type in multi-line lists without having to
;; type in continuation characters.
(defvar *reading-list* nil)

;; For translator.
(defvar *procedure-text* "")
(defvar *defining-procedure* nil)
(defvar *current-procedure* nil)
(defvar *undefineds* (make-hash-table))
(defvar *instruction* nil)
(defvar *remainder* nil)

;;; Turtle graphics.
(defvar *display* nil)
(defvar *screen* nil)
(defvar *root* nil)

(defvar *turtle-window* nil)
(defparameter *window-width* 800)
(defparameter *window-height* 800)

(defvar *black-pixel* nil)
(defvar *white-pixel* nil)

(defvar *colormap* nil)
(defparameter *color-names* 
  #("black" "blue" "green" "cyan" "red" "magenta" "yellow" "white"
    "brown" "tan" "forest green" "aquamarine" "salmon" "purple" "orange" "grey"))
(defvar *color-array* nil)

;;; Scrunch allows customizing aspect ratio. It also allows a
;;; convenient way to scale pictures.
(defvar *scrunch-x* 1)
(defvar *scrunch-y* 1)

(defvar *turtles* nil)   ; XXX This should be an array.
(defvar *current-turtle* nil)
(defvar *turtle-draw-turtle* nil)
(defvar *deferred-redraw* nil)

;;; Fonts
(defparameter *turtle-tiny-font* "-adobe-new century schoolbook-medium-r-*-*-8-*-*-*-*-*-*-*")     ; 8pt
(defparameter *turtle-small-font* "-adobe-new century schoolbook-medium-r-*-*-10-*-*-*-*-*-*-*")   ; 10pt
(defparameter *turtle-normal-font* "-adobe-new century schoolbook-medium-r-*-*-12-*-*-*-*-*-*-*")   ; 12pt
(defparameter *turtle-large-font* "-adobe-new century schoolbook-medium-r-*-*-18-*-*-*-*-*-*-*")    ; 16pt
(defparameter *turtle-huge-font* "-adobe-new century schoolbook-medium-r-*-*-24-*-*-*-*-*-*-*")     ; 20pt

(defvar *draw-gc*)
(defvar *turtle-draw-gc*)
(defvar *turtle-paint-gc*)
(defvar *turtle-erase-gc*)
(defvar *turtle-reverse-gc*)
(defvar *turtle-up-gc*)

;;; Debugging
(defparameter *debug* nil)
(defparameter *error-debug* nil)

(defun dformat (&rest args)
  (when *debug*
    (apply #'format (cons *debug-io* args))))

(defun eformat (&rest args)
  (when *error-debug*
    (apply #'format (cons *error-output* args))))

(defparameter *debug-infix* nil)

(defun diformat (&rest args)
  (when *debug-infix*
    (apply #'format (cons *debug-io* args))
    (force-output *debug-io*)))

(defparameter *break-on-errors* nil)

