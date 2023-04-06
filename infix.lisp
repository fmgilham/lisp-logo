;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-16 12:10:25 fred>
;;; Logo-to-lisp translator: infix.lisp
;;; Code to translate from infix to prefix.
;;; Guts stolen from Winston & Horn, LISP, 3rd edition, pg. 488-489.
;;; Original: FMG 10-AUG-1999
;;;
;;; This is not robust. Parenthesization and runlists seem to confuse
;;; it at times.

(in-package "LOGO")

(declaim #.*compile-options*)

(defun infix-p (thing)
  "Check if thing is an infix operation."
  (and (symbolp thing) (procedure-infix-or-weight thing)))

;;; Note Well: PREFIX-P is not simply the negation of INFIX-P. The
;;; checks are necessary to avoid calling numbers etc. prefix
;;; operations.
(defun prefix-p (thing)
  "Check if thing is a prefix procedure."
  (and (symbolp thing)
       (let ((p (find-procedure-from-symbol thing)))
	 (diformat "procedure is ~A~%" p)
	 (when p
	   (not (or (logo-procedure-infix p)
		    (zerop (logo-procedure-input-count p))))))))

(defun infix-exp-p (thing)
  "Check if thing is an infix expression."
  (destructuring-bind (first &optional op second &rest stuff) thing
      (declare (ignore stuff))
      (and first op second ; Make sure we have an infix expression in form.
	   (infix-p op)    ; Make sure the operator is an infix op.
	   )))

(defun procedure-infix-or-weight (p)
  (let ((pp (find-procedure-from-symbol p)))
    (when pp
      (logo-procedure-infix pp))))

(defun weight (op)
  "Get the precedence weight of the operation. Returns nil if the
operation isn't an infix operator."
  (procedure-infix-or-weight op))

;; Winston and Horn used AE for the argument here and in what follows.
;; I had to dig up the book to figure out what ae stood for
;; (admittedly it was pretty obvious). Anyway I'm stupid so I changed
;; AE to EXPRESSION.
(defun inf-to-pre (expression)
  "Translate an infix expression to prefix."
  (diformat "inf-to-pre: expression: ~A~%" expression)
  (cond ((or (atom expression) #-(and) (quoted-p expression))   ; Check for easy case.
	 (diformat "inf-to-pre: atom~%")
	 ;; Is this a prefix procedure?
	 (let ((p (find-procedure-from-symbol expression)))
	   (if (and p (zerop (logo-procedure-input-count p)))
	       (translate-procedure-call (cl:list expression))
	       expression)))
	((and (cl:listp expression)
	      (prefix-p (car expression)))
	 ;; Prefix expression
	 (diformat "inf-to-pre: prefix~%")
	 (if (zerop (logo-procedure-input-count
		     (find-procedure-from-symbol (car expression))))
	     ;; The "expression" takes no inputs.
	     (values expression (translate-procedure-call (cdr expression)))
	     ;; The expression is a list that is a procedure call. Return it.
	     (translate-procedure-list-call expression)))
	(t
	 (diformat "inf-to-pre: default~%")
	 (inf-aux expression nil nil))))                ; Start with empty stacks.

(defun inf-aux (expression operators operands)
  (diformat "inf-aux: expression: ~A~%" expression)
  (diformat "inf-aux: operators: ~A~%" operators)
  (diformat "inf-aux: operands: ~A~%" operands)
  (if (prefix-p (car expression))
      ;; If the current expression list starts with a prefix
      ;; expression, handle it.
      (progn
	(diformat "inf-aux: prefix-p, ~A~%" (car expression))
	(multiple-value-bind (pe rest) (translate-procedure-call expression)
	  (inf-iter rest operators (cons pe operands))))
      (progn
	(diformat "inf-aux: not prefix-p, ~A~%" (car expression))
	(if (quoted-p expression)
	    ;; This will be an error but we'll catch it later.
	    (cons (inf-to-pre (car expression)) operands)
	    (inf-iter (cdr expression)
		      operators
		      (cons (inf-to-pre (car expression)) operands))))))

(defun inf-iter (expression operators operands)
  (cond ((and (endp expression) (endp operators))	; Termination?
	 (car operands))				; Result.
	((and (not (endp expression))			; Not end of EXPRESSION?
	      (or (endp operators)			; Empty stack?
		  (and (not (prefix-p (car expression)))
		       (> (weight (car expression))	; Compare weights.
			  (weight (car operators))))))
	 (inf-aux (cdr expression)
		  (cons (car expression) operators)
		  operands))
	(t (inf-iter expression
		     (cdr operators)                   ; Pop operator.
		     (cons (cl:list (car operators)
				    (cadr operands)      ; Construct
                                    (car operands))      ; result.
			   (cdr (cdr operands)))))))  ; Pop operands.


(defun collect-infix (obj list)
  "Extract the infix part of the list and return it as a list (with obj
as the first element of the returned list) and the remainder as a
second value."
  (let ((result (cl:list obj (car list)))
	(rest (cdr list)))
    (unless rest
      (format *error-output* "Not enough inputs to ~A~%" (car list))
      (throw 'logo-non-fatal nil))
    (multiple-value-bind (infix left-over) (collect-infix-first-is-obj rest)
      (values (append result infix) left-over))))


(defun collect-infix-first-is-obj (list)
  "Helper for collect-infix. Takes a list whose first element is an
object (as opposed to an operator) and collects the rest of the infix
stuff from that list."
  (let ((obj (car list))
	(maybe-op (cadr list)))
    (cond ((weight maybe-op)
	   (collect-infix obj (cdr list)))
	  ((prefix-p obj)
	   (multiple-value-bind (pe rest)
	       (translate-procedure-call list)
	     ;; Can't throw away the procedure name yet.
	     ;; Fixes bug 3 + first :foo where :foo contains a list
	     (values (list* obj (cdr pe)) rest)))
	  (t
	   (values (cl:list obj) (cdr list))))))
    

(defun process-infix (obj list)
  "This is the main interface to the infix translation stuff. It takes
an object (either a number or a possibly numeric expression), an infix
operator, and a list containing the second argument of the infix
operation. It returns a prefix expression incorporating everything in
the list that was part of the infix expression and a second value of
the remainder of the list. XXXX this documentation string is incorrect
as it stands, but it describes what should be done."
  (multiple-value-bind (exp rest) (collect-infix obj list)
    (values (inf-to-pre exp) rest)))
    
(defun infix-trace-all ()
  (cl:trace process-infix collect-infix collect-infix-first-is-obj
	 inf-to-pre inf-aux inf-iter))

(defun infix-untrace-all ()
  (cl:untrace process-infix collect-infix collect-infix-first-is-obj
	      inf-to-pre inf-aux inf-iter))
