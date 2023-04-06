;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-14 07:45:11 fred>
;;; Logo-to-lisp translator: logo-procedure structure and utilities.


(in-package "LOGO")

(declaim #.*compile-options*)

(defstruct logo-procedure
  namestring			       ; The procedure name string.
  user-name                            ; The user-typed procedure name (for error reporting).
  (procedure nil :type symbol)         ; The symbol for that procedure.
  (built-in t :type (member t nil))    ; T if the procedure is "primitive" or built-in
  (input-count 0 :type fixnum)         ; Number of inputs the procedure takes.
  (called-by nil)                      ; Procedures that call this procedure
  (kind :undefined :type (member :undefined :operator :command :special))
  (infix nil :type (or null fixnum))   ; T if the operator is an infix operator.
  (accepts-list nil)                   ; True if procedure will take a list of inputs.
  (text nil)                           ; The text of the procedure, if available.
  (buried t))                          ; A buried procedure can't be redefined.

;;;
;;; Procedure definition and lookup
;;;

(defun def-the-procedure (namestring user-name procedure built-in input-count called-by kind
			  infix accepts-list text buried)
  (let* ((*package* (find-package "LOGO"))
	 (procedure-sym (make-logo-symbol (string-upcase procedure)))
	 (procedure (make-logo-procedure :namestring namestring
			       :user-name user-name
			       :procedure procedure-sym
			       :built-in built-in
			       :input-count input-count
			       :called-by called-by
			       :kind kind
			       :infix infix
			       :accepts-list accepts-list
			       :text text
			       :buried buried)))
    (export procedure-sym *package*)
    (setf (gethash namestring *procedure-names*) procedure)
    (setf (gethash procedure-sym *procedure-symbols*) procedure)))


(defun def-logo-primitive (namestring procedure input-count
			   kind infix accepts-list)
  (def-the-procedure namestring nil procedure t input-count nil kind infix accepts-list nil t))

(defun def-proc (namestring
		 user-name
		 procedure
		 input-count
		 &key
		   called-by 
		   kind
		   infix
		   accepts-list
		   text
		   buried)
  (def-the-procedure namestring user-name procedure nil input-count called-by kind infix accepts-list text buried))

;; Lookup a procedure symbol (convert to a string first) or string.
(defun find-procedure-from-name (thing)
  "Look up a procedure structure from the string or symbol of its namestring."
  (let ((name (typecase thing
		(string (string-upcase thing))
		(symbol (string-upcase (symbol-name thing)))
		(otherwise nil))))
    (gethash name *procedure-names*)))


(defun find-procedure-from-symbol (thing)
  "Look up a procedure structure from the function symbol."
  (gethash thing *procedure-symbols*))


;; This is for "backward compatibility."
(defun find-procedure (thing)
  (and (or (stringp thing) (symbolp thing))
       (or (find-procedure-from-name thing)
	   (if (symbolp thing)
	       (find-procedure-from-symbol thing)
	       (find-procedure-from-symbol (make-logo-symbol (string-upcase thing)))))))


(defun operator-p (procedure)
  (and (logo-procedure-p procedure)
       (eq (logo-procedure-kind procedure) :operator)))

(defun command-p (procedure)
  (and (logo-procedure-p procedure)
       (eq (logo-procedure-kind procedure) :command)))

(defun special-p (procedure)
  (and (logo-procedure-p procedure)
       (eq (logo-procedure-kind procedure) :special)))


;;;
;;; For debugging.
;;;

(defun dump-procedures (&optional (built-in nil))
  (maphash 
   #'(lambda (name proc)
       (when (or built-in	     ; If built-in is T, dump them all
		 (not (logo-procedure-built-in proc))) ; only dump user-defined
	 (format *debug-io* "~A:: ~A ~%" name proc)))
   *procedure-names*))

(defun dump-primitives ()
  (maphash
   #'(lambda (name proc)
       (when (logo-procedure-built-in proc)
	 (format *debug-io* "~A:: ~A ~%" name proc)))
   *procedure-names*))

(defun dump-operators (&optional (built-in t))
  (maphash
   #'(lambda (name proc)
       (when (eq (logo-procedure-kind proc) :operator)
	 (when (or (and built-in (logo-procedure-built-in proc))
		   (and (not built-in) (not (logo-procedure-built-in proc))))
	   (format *debug-io* "~A:: ~A ~%" name proc))))
   *procedure-names*))

(defun dump-commands (&optional (built-in t))
  (maphash
   #'(lambda (name proc)
       (when (eq (logo-procedure-kind proc) :command)
	 (when (or (and built-in (logo-procedure-built-in proc))
		   (and (not built-in) (not (logo-procedure-built-in proc))))
	   (format *debug-io* "~A:: ~A ~%" name proc))))
   *procedure-names*))

;; All specials are built-in so we don't need to worry about that now.
(defun dump-specials ()
  (maphash
   #'(lambda (name proc)
       (when (eq (logo-procedure-kind proc) :special)
	 (format *debug-io* "~A:: ~A ~%" name proc)))
   *procedure-names*))

(defun print-logo-symbols () 
   (with-package-iterator (next-symbol (find-package "LOGO") :external)
     (loop
       (multiple-value-bind (more? symbol) (next-symbol)
         (if more? 
            (print symbol)
            (return))))))
