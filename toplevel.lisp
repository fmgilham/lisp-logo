;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; $Id: toplevel.lisp,v 1.6 2022/10/18 00:09:36 fred Exp $
;;; $Source: /alpha/homes/fred/lisp/mylogo/lisp/RCS/toplevel.lisp,v $
;;; Logo-to-lisp translator: toplevel.lisp
;;; This file implements a toplevel `evaluator'.
;;; Original: FMG 8-AUG-1999

(in-package "LOGO")

(declaim #.*compile-options*)

;;
;; Stolen from Garnet's protected eval subsystem.
(defmacro with-abort (&body forms)
  "Executes forms in an environment in which there exists an abort
restart.  The abort restart returns two values, nil and :abort"
  (let ((abort-block-tag (gensym "ABORT")))
    `(block ,abort-block-tag
       (restart-case (progn ,.forms)
         (abort () :report "Abandon Computation, Return nil"
           (return-from ,abort-block-tag (values nil :abort)))))))

(defmacro with-interrupt (&body forms)
  "Executes forms in an environment in which there exists a restart
for interrupts."
  (let ((interrupt-block-tag (gensym "INTERRUPT")))
    `(block ,interrupt-block-tag
       (restart-case (progn ,.forms)
         (interrupt () :report "Stop Computation, Return nil"
           (return-from ,interrupt-block-tag (values nil :interrupt)))))))



(defun init-logo ()
  "Do whatever inits are necessary to start, restart or update logo
after every cycle of the toplevel loop."
  (setup-logo-readtable)
  (setf *current-procedure* nil)
  ;; for turtle. Just notice resizing window for now.
  #+xlib
  (update-window-system-state))

(defun cleanup (&optional all)
  "Do whatever inits are necessary to start or restart logo."
  (cleanup-readtable)
  (setf *defining-procedure* nil)
  (setf *current-procedure* nil)
  #+xlib
  (when all
    (cleanup-graphics)))     ; In turtle.lisp

;;;
;;; Logo Read/Eval loop (printing is only done on command).
;;;
;;; XXX Interrupts don't work. XXX This is a bit of a mess.
;;;
(defun logo-toplevel ()
  "Top-level logo interpreter loop."
  (block logo-toplevel
    (version)			; Print the "herald".
    (let ((*package* (find-package "LOGO")))
      (unwind-protect
	   (catch 'logo-exit
	     (let ((logo-fatal-error
		     (catch 'logo-fatal ;; Errors that should probably cause exit.
		       (with-abort
			 (loop
			   (let ((non-fatal-return
				   (catch 'logo-non-fatal
				     (let ((thing
					     (catch 'not-in-procedure
					       (with-logo-error-trapping
						 (init-logo)
						 (let ((instructions (collect-all-instructions)))
						   (logo-eval instructions)
						   nil)))))
				       (when thing
					 (format t "~&Can only use ~A inside a procedure.~%" thing))))))
			     (when (eq non-fatal-return 'lisp-escape)
			       (let ((*readtable* (copy-readtable nil)))
				 #+sbcl (return-from logo-toplevel nil)
				 #+cmu (lisp::%top-level) ;; XXX The source says "don't call this". Why not?
				 #+ccl (ccl:toplevel)
				 ))))))))
	       (when logo-fatal-error
		 (cl:print logo-fatal-error)
		 (throw 'logo-exit nil))))
	(cleanup))
      (cleanup t)
      #+sbcl (sb-ext:exit)
      #+cmu (ext:quit)
      #+ccl (ccl:quit))
    ))


(defun logo-eval (instructions)
  "Evaluate one or more logo instructions."
  (let* ((tmp-name (gensym))
	 (tmp-def `(lambda ()
		     (declare (optimize (speed 3) (safety 1) (debug 2) #+cmu (ext:inhibit-warnings 3)))
		     ,instructions)))
    (dformat "~&def: ~A~%" tmp-def)
    ;;
    ;; Surround everything that calls the compiler with this noise
    ;; (see e.g. run, to).
    (unwind-protect
	 (#+sbcl sb-ext:without-package-locks 
	  #-sbcl progn
	  #+sbcl (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note))
	  #+cmu (setf cl:*compile-print* nil)
	  (let* ((fun (compile tmp-name tmp-def))
		 (badreturn (funcall fun)))
	    (when badreturn ;; If there is a non-nil return, warn.
	      (let ((*current-stream* *error-output*))
		(declare (special *current-stream*))
		(format *error-output* "~&You don't say what to do with ")
		(logo::show badreturn)))))
      (progn
	(setf *defining-procedure* nil)
	#+sbcl (proclaim '(sb-ext:unmuffle-conditions sb-ext:compiler-note))
	#+cmu (setf cl:*compile-print* t)))
    ))
