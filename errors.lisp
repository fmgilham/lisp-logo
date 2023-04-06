;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-14 07:45:00 fred>
;;; Logo-to-lisp translator: errors.lisp
;;; Logo error stuff.  Centralize reporting and messages.
;;; Original: FMG 8-AUG-1999
;;;


(in-package "LOGO")

(declaim #.*compile-options*)

;;;
;;; Logo error system utilities. This still needs work.
;;;
;;; TODO: Do something better with lisp-level errors.
;;;       In general do a better job of reporting where the error occurred.
;;;       Try, at all costs, to keep the user out of the lisp debugger.
;;;       Logo-level debugging?
;;;       In general perhaps rethink this entire scheme.
;;;       A couple places where errors are reported without using this
;;;       stuff---consolodate.
;;;
(defmacro with-logo-error-trapping (&body forms)
  "Evaluate forms in the context of handlers that trap logo errors."
  `(restart-case
       (handler-bind 
	   (#+sbcl
	    ;; This interacts (pun not intended) poorly with slime.
	    ;; Need to establish a restart that allows return to logo
	    ;; toplevel after interrupt.
	    ;;;
	    ;;; XXX I can't get this to work. I want it to
	    ;;; automatically invoke the interrupt restart, not go
	    ;;; into the deugger.
	    #-(and)
	    (sb-sys:interactive-interrupt #'handle-interrupt)
;;; Trapping this condition breaks compiling. Something's screwy with
;;; the way cmucl compiles functions.
;;;         #+cmu
;;;	    (simple-condition #'handle-nonfatal)
	    (logo-unbound-variable      #'handle-nonfatal)
	    (logo-undefined-procedure   #'handle-nonfatal)		  
	    (logo-redefine-primitive    #'handle-nonfatal)
	    (logo-defining-error        #'handle-nonfatal)		  
	    (logo-type-error            #'handle-nonfatal)
	    (logo-not-enough-inputs     #'handle-nonfatal)
	    (logo-nonfatal-error        #'handle-nonfatal)
	    (logo-error                 #'handle-fatal)
	    (unbound-variable           #'handle-nonfatal)
	    (undefined-function         #'handle-nonfatal)
	    (error                      #'handle-nonfatal))
	 ,@forms)
     (interrupt-restart (&optional v)
       :report "Interrupt"
       v)))
  


(defun handle-interrupt (condition)
  (declare (ignore condition))
  (format *error-output* "~&Interrupt...~%")
  (force-output *error-output*)
  (invoke-restart 'interrupt-restart)
  )
  


(defun handle-nonfatal (condition)
  (eformat "~&Logo nonfatal error: ")
  (force-output *error-output*)
;;  (cl:print condition *error-output*)
  (if *break-on-errors*
      (break))
  (report-errors condition)
  (throw 'logo-non-fatal nil))

(defun handle-fatal (condition)
  (eformat "~&Logo fatal error: ")
  (force-output *error-output*)
  (report-errors condition)
  (throw 'logo-fatal nil)
)

(defun handle-system (condition)
  (format *error-output* "~&Logo doesn't understand what you did and got a system error: ")
  (force-output *error-output*)
  (report-errors condition)
  (throw 'logo-non-fatal nil))


(defun report-backtrace-list (list)
  (if (null list)
      nil
      (progn
	(format *error-output* "In ~A > " (logo-procedure-namestring (car list)))
	(report-backtrace-list (cdr list)))))

#+sbcl
(defun report-backtrace ()
  (do* ((frames (sb-debug:list-backtrace) (cdr frames))
	(frame (car frames) (car frames))
	(function (car frame) (car frame))
	(logo-procedure (find-procedure-from-symbol function) 
			(find-procedure-from-symbol function))
	(backtrace-list nil))
       ((null frames) (report-backtrace-list backtrace-list))
    (when logo-procedure
      (pushnew logo-procedure backtrace-list))))

#+ccl
(defun report-backtrace ()
  (do* ((frames (ccl:backtrace-as-list) (cdr frames))
	(frame (car frames) (car frames))
	(function (car frame) (car frame))
	(logo-procedure (find-procedure-from-symbol function) 
			(find-procedure-from-symbol function))
	(backtrace-list nil))
       ((null frames) (report-backtrace-list backtrace-list))
    (when logo-procedure
      (pushnew logo-procedure backtrace-list))))

#+cmu 
(defun report-backtrace ()
  (do* ((frame (di:top-frame) (di:frame-down frame))
	(backtrace-list nil))
       ((null frame) (report-backtrace-list backtrace-list))
    (let* ((d-fun (di:frame-debug-function frame))
	   (function (di:debug-function-name d-fun))
	   (logo-procedure (find-procedure-from-symbol function)))
      (when logo-procedure
	(pushnew logo-procedure backtrace-list)))))


;;; Kinda bogus. Should probably re-raise these errors as the logo versions.
(defun report-errors (condition)
  #+(or sbcl ccl cmu)
  (report-backtrace)
  (cond ((eql (type-of condition) 'unbound-variable)
	 (format *error-output* "~&~A has no value.~%"
		 (cell-error-name condition)))
	((eql (type-of condition) 'undefined-function)
	 (format *error-output* "~&I don't know how to ~A.~%"
		 (cell-error-name condition)))
	((eql (type-of condition) 'type-error)
	 (format *error-output* "~&I don't like the input ~A. I expected a ~A.~%"
		 (type-error-datum condition) (type-error-expected-type condition)))
	(t
	 (format *error-output* "~&~A~%" condition)))
  (when *current-procedure* (format *error-output* "(Current procedure is ~A)~%" *current-procedure*))
  (force-output *error-output*))


#+cmu
(defun scan-backtrace (count)
  (do ((frame (or debug::*current-frame* (di:top-frame)) (di:frame-down frame))
       (index 0 (1+ index)))
      ((> index count))
    (di:debug-function-name (di:frame-debug-function frame))))

#-(and)
(defun scan-backtrace (count)
  (do ((frame (or debug::*current-frame* (sb-di:top-frame)) (sb-di:frame-down frame))
       (index 0 (1+ index)))
      ((> index count))
    (sb-introspect:debug-info-debug-function frame)))





;;;
;;; Logo error conditions.
;;;
(define-condition logo-error (error)
  ((logo-error-procedure-name :reader logo-error-procedure-name :initarg :procedure-name))
  (:report (lambda (condition stream)
	     (declare (ignore condition))
             (format stream "~&Logo: Fatal Internal Error.~%
(in other words, you did something the person who wrote~%
this Logo system didn't expect anyone ever to do)."))))


(define-condition logo-nonfatal-error (logo-error)
  nil
  (:report (lambda (condition stream)
	     (declare (ignore condition))
             (format stream "~&Logo: Non-fatal error: restarting.~%
(in other words, Logo didn't understand what you tried to do, but~%
we think we can recover without killing the whole system)."))))

(define-condition logo-unbound-variable (logo-nonfatal-error unbound-variable)
  nil
  (:report (lambda (condition stream)
             (format stream "~&~A has no value in ~A"
		     (cell-error-name condition)
		     (logo-error-procedure-name condition)))))


(define-condition logo-undefined-procedure (logo-nonfatal-error undefined-function)
  nil
  (:report (lambda (condition stream)
             (format stream "~&I don't know how to ~A."
		     (cell-error-name condition))
	     )))


(define-condition logo-redefine (logo-nonfatal-error)
  ((logo-error-procedure-name :reader logo-error-procedure-name :initarg :procedure-name))
  (:report (lambda (condition stream)
             (format stream "~&Redefinition error:: ~A."
		     (logo-error-procedure-name condition))
	     (abort)
	     )))

(define-condition logo-redefine-primitive (logo-redefine)
  ((logo-error-procedure-name :reader logo-error-procedure-name :initarg :procedure-name))
  (:report (lambda (condition stream)
             (format stream "~&You can't redefine a built-in procedure: ~A."
		     (logo-error-procedure-name condition))
	     (abort)
	     )))

(define-condition logo-redefine-buried (logo-redefine)
  ((logo-error-procedure-name :reader logo-error-procedure-name :initarg :procedure-name))
  (:report (lambda (condition stream)
             (format stream "~&You can't redefine a buried procedure: ~A."
		     (logo-error-procedure-name condition))
	     (abort)
	     )))

(define-condition logo-not-enough-inputs (logo-nonfatal-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~&Not enough inputs to ~A."
		     (logo-error-procedure-name condition)))))


(define-condition logo-type-error (logo-nonfatal-error type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
		     "~&~A doesn't like ~A as input.~%It wants a ~A."
		     (logo-error-procedure-name condition)
		     (type-error-datum condition)
		     (type-error-expected-type condition)))))

(define-condition logo-defining-error (logo-nonfatal-error)
  ((argument :reader logo-defining-error-argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream
		     "~&Can't use TO inside a procedure.~%You tried to do this while defining ~A."
		     (logo-defining-error-argument condition)))))

(define-condition logo-invalid-usage (logo-nonfatal-error)
  ((argument :reader logo-invalid-word :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "~&Can only use ~A inside a procedure."
		     (logo-invalid-word condition)))))

(define-condition logo-out-of-bounds (logo-nonfatal-error)
  ((coordinate :reader coordinate  :initarg :coordinate)
   (axis :reader axis  :initarg :axis))
  (:report (lambda (condition stream)
             (format stream "~&Fence mode: the turtle has gone out of bounds on the ~A axis. Value: ~A"
		     (axis condition)
		     (coordinate condition)))))


(define-condition logo-file-error (logo-nonfatal-error file-error)
  ()
  (:report (lambda (condition stream)
             (format stream
		     "~&I can't open file ~A."
		     (file-error-pathname condition)))))
