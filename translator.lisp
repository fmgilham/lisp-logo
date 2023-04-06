;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; $Id: translator.lisp,v 1.7 2022/10/18 00:09:36 fred Exp $
;;; $Source: /alpha/homes/fred/lisp/mylogo/lisp/RCS/translator.lisp,v $
;;; Logo-to-lisp translator: translator.lisp
;;; This file contains the code to translate a list of objects read from
;;; logo syntax into lisp forms.
;;; Original: FMG 8-AUG-1999
;;;
;;; The code in this file seems excessively complicated and redundant.
;;;
;;; The job of this code is to translate lines of logo symbols into
;;; valid lisp using lisp forms. This is aided and abetted by the code
;;; in procedures.lisp.
;;;
;;; For example, a TO form becomes a macro call to the TO macro in
;;; procedures.lisp. LOCAL and LOCALMAKE get intercepted in the code
;;; in procedures.lisp and turned into LET forms.
;;;
;;; Logo absolutely depends on knowing the number of inputs for each
;;; function. This allows the parser to break up a line into single
;;; instructions. However, an undefined procedure has an unknown
;;; number of inputs. So we try to figure out how many inputs go with
;;; the undefined procedure by scanning the line for the first call to
;;; a procedure that could not be an input because it doesn't output
;;; anything.
;;;
;;; We then subtract the number of inputs for each procedure that in
;;; the line that could be an input from the total count of possible
;;; inputs (because the defined procedures will consume their number
;;; of inputs). This should give us the total number of inputs for the
;;; undefined procedure.

(cl:in-package "LOGO")

(declaim #.*compile-options*)



(defmacro thing (thing)
  "Variable access."
  (dformat "Thing is ~A~%" thing)
  `(cl:symbol-value ,thing))

(defun thing-p (obj)
  (and (consp obj) (eq (car obj) 'thing)))


;;
;; Try to translate a list of logo instructions.
;;
(defun translate-instruction-list (instruction-list)
  "Translate a list of instructions. Return two values: an instruction
and the part of the list that wasn't consumed in this instruction."

  (dformat "~&Translating instruction list ~A~%" instruction-list)

  (cond
    ((null instruction-list)
     (error "Null instruction list passed to TRANSLATE-INSTRUCTION-LIST."))

    ;; Maybe a procedure that takes no inputs, or a number. Actually
    ;; I'd be surprised if this case ever occurs. But this code is
    ;; constantly surprising me.
    ((atom instruction-list)
     (dformat "~&instruction list is atom: ~A~%" instruction-list)
     (error "Invalid instruction list passed to TRANSLATE-INSTRUCTION-LIST: ~A" instruction-list))

    ((infix-exp-p instruction-list)
     (dformat "~&Infix instruction list: ~A~%" instruction-list)
     (multiple-value-bind (instruction remainder)
	 (process-infix (car instruction-list) (cdr instruction-list))
       (values instruction remainder)))
    
    (t
     (let ((head-item (car instruction-list))
	   (rest (cdr instruction-list)))
       (dformat "head-item: ~A~%" head-item)
       (dformat "Rest: ~A~%" rest)
       
       ;; Dispatch on the type of the procedure or form
       (cond

	 ;; This handles a list where the first item is a number. It
	 ;; interfaces to the infix translation code if there are
	 ;; more elements following the number.
	 ((cl:numberp head-item)
	  (dformat "head-item is number.~%")
	  (if (and (cdr instruction-list)
		   (infix-p (cadr instruction-list)))
	      (multiple-value-bind (instruction remainder)
		  (process-infix head-item rest)
		(values instruction remainder))
	      (values head-item rest)))

	 ;; Quoted thing, I.e. "a or [a b c]. Just return it.
	 ((quoted-p head-item)
	  (dformat "head-item is word or quoted list: ~A~%" head-item)
	  (values head-item rest))

	 ;; Variable reference
	 ((thing-p head-item)
	  ;; If the next element is an infix operator, process as infix.
	  (if (and (cdr instruction-list)
		   (infix-p (cadr instruction-list)))
	      (multiple-value-bind (instruction remainder)
		  (process-infix head-item rest)
		(values instruction remainder))
	      (values (thing (ref-place head-item)) rest)))

	 ((eq head-item 'local)
	  ;; Don't mess with it. It's already in the right form.
	  (values instruction-list nil))
	 
	 ;; list. I.e. something like (sum 1 2 3 4). Use consp because
	 ;; listp will be true for nil.
	 ((consp head-item)
	  (dformat "head-item is list. Car is ~A~%" (car head-item))
	  (let ((procedure (find-procedure-from-symbol (car head-item))))
	    (if (and procedure (logo-procedure-accepts-list procedure))
		;; We have a procedure that takes a list.
		(progn
		  (dformat "Procedure accepts list.~%")
		  (values (translate-procedure-list-call head-item) rest))
		;; probably multiple instructions in one line
		(progn
		  (dformat "Probably multiple instructions in one line.~%")
		  (let ((instruction (translate-instruction-list head-item)))
		    (values instruction rest))))))

	 ;; Logo procedure call.
	 (t
	  (dformat "head-item is procedure ~A.~%" head-item)
	  (translate-procedure-call instruction-list)))))))




(defun translate-procedure-list-call (instruction-list)
  "Handle a built-in procedure that takes a list of inputs.
We know that the procedure is built-in and that it takes a list of
inputs because that's the only way we get called."
  (do* ((current-object (car instruction-list))
	(remainder (cdr instruction-list))
	(procedure (find-procedure-from-symbol current-object))
	(input-list nil)
	input)
       ((null remainder)
	(values (cons (logo-procedure-procedure procedure) (nreverse input-list)) nil))
    (dformat "List as procedure call: ~A~%" current-object)
    (dformat "Remainder: ~A~%" remainder)
    
    (multiple-value-setq (input remainder)
      (get-one-input remainder))
    (when input
      (push input input-list))))



(defun translate-procedure-call (instruction-list)
  "Translate a list representing a procedure call into the lisp equivalent."
  (let* ((current-object (car instruction-list))
	 (rest-of-instruction (cdr instruction-list))
	 (procedure (find-procedure-from-symbol current-object)))

    (dformat "translate-procedure-call current-object: ~A~%" current-object)
    (dformat "translate-procedure-call rest-of-instruction: ~A~%" rest-of-instruction)
    
    ;; Check for calling undefined procedure. If we're defining a
    ;; procedure, allow undefined procedures.
    ;;
    ;; Note that we don't need
    ;; to check for this error here but if we do we can make the error
    ;; message print what the user typed.
    ;; I.e.
    ;; `I don't know how to Baz'
    ;; instead of
    ;; `I don't know how to BAZ'.

    ;; Deal with undefined procedure calls here.
    (unless procedure
      ;; If we are NOT defining a procedure, an undefined procedure
      ;; call is an error.
      (unless (defining-procedure)
	(error 'logo-undefined-procedure :name current-object))

      ;; Debug information for when we encounter an undefined
      ;; procedure inside a procedure being defined.
      (dformat "Undefined procedure ~A while defining ~A~%"
	       current-object *current-procedure*)
      (dformat "Rest of instruction is ~A, length ~A~%"
	       rest-of-instruction (length rest-of-instruction))

      ;; Handle undefined procedure calls while defining a new
      ;; procedure by making a dummy procedure for the undefined
      ;; procedure. Try to get the input count for the dummy
      ;; procedure by scanning the rest of the instruction line.
      ;; Stop when we encounter the first command because
      ;; commands don't output a value and so can't be an input
      ;; to the undefined procedure.
      ;;
      ;; The assumption is that this procedure will be redefined
      ;; later when the actual definition is loaded or typed in.
      (let* ((input-list (subseq rest-of-instruction 0 (position-if
							(lambda (thing)
							  (let ((proc (find-procedure thing)))
							    (and proc (command-p proc))))
							rest-of-instruction)))
	     (input-count (length input-list))
	     ;; Here we subtract out the number of inputs for each operator we find.
	     ;; I.e. an input list of 3 4 SUM 5 6 consists of 3 inputs not five because
	     ;; SUM swallows two of the items in the list.
	     (actual-count (dolist (input input-list input-count)
			     (let ((proc (find-procedure input)))
			       (when proc (decf input-count (logo-procedure-input-count proc)))))))
		     
	(setf procedure (def-proc
			    (string-upcase current-object) current-object
			  (read-from-string (format nil "~A" current-object) nil nil)
			  actual-count
			  :called-by (cl:list *current-procedure*)
			  :kind :undefined
			  :buried nil))))

    ;; At this point we should have a procedure and a pretty
    ;; accurate, though perhaps guesstimated, input count for
    ;; that procedure. Now process inputs in the procedure call
    ;; instruction list.
    (let ((input-count (logo-procedure-input-count procedure))
	  (input-list nil)
	  (kind (logo-procedure-kind procedure))
	  (input-length (length rest-of-instruction)))
      ;; Check number of arguments. Note that "infix" calls have two
      ;; arguments, but one should already have been processed.
      (unless (or (and procedure (logo-procedure-infix procedure) (>= input-length 1))
		  (<= input-count input-length))
	(error 'logo-not-enough-inputs :procedure-name current-object))

      (cond
	;; Check MAKE --- the first input must be a name
	((eq current-object 'make)
	 (let ((name (car rest-of-instruction)))
	   (unless (quoted-p name)
	     (error 'logo-type-error 
		    :procedure-name current-object
		    :datum (if (eq (caar rest-of-instruction) 'thing)
			       (format nil ":~A" (unquote (cadar rest-of-instruction)))
			       (format nil "~A" (car rest-of-instruction)))
		    :expected-type (if *current-procedure*
				       (format nil 
					       "word.~%(This happened while defining ~A)"
					       (string-upcase *current-procedure*))
				       "word")))
	   (multiple-value-bind (value rest)
	       (get-one-input (cdr rest-of-instruction))
	     (values `(make ,name ,value) rest))))
	((eq current-object 'to)
	 (dformat "Translate-procedure-call to form: ~A.~%" instruction-list)
	 (when (eq current-object 'to)
	   (setf *current-procedure* (cadr instruction-list))
	   (values instruction-list nil)))
	(t
	 (dformat "Current object is ~A; current instruction list is ~A~%Input count is ~A~%" 
		  current-object instruction-list input-count)
	 (let ((rest rest-of-instruction)
	       input)
	   (dotimes (i input-count (setq rest-of-instruction rest))
	     (multiple-value-setq (input rest)
	       (get-one-input rest))
	     (when input (push input input-list))))

	 (dformat "~&Done getting inputs.~%")

	 (setf input-list (nreverse input-list))
	 (values (cons (if (eq kind :undefined)
			   current-object
			   (logo-procedure-procedure procedure))
		       input-list)
		 rest-of-instruction))))))


;; Call GET-ONE-INPUT with a list. GET-ONE-INPUT at the head of the
;; list. If it's a procedure, it calls TRANSLATE-PROCEDURE-CALL, which
;; may invoke GET-ONE-INPUT but we don't care about that. Otherwise
;; GET-ONE-INPUT swallows one data object and returns that object as its
;; first value and the rest of the list as the second value.
;;
;; It's an error to call GET-ONE-INPUT with an empty OBJECT-LIST.
(defun get-one-input (object-list)
  "Swallow up as much of OBJECT-LIST as necessary to make a single
input, and return that as the first value, and whatever is left as the
second value."

  (unless (cl:listp object-list)
    (error "~&get-one-input called with non-list: ~A~%" object-list))
  
  (dformat "~&Get-one-input: object list ~A~%" object-list)

  (when (null object-list)
    ;; This is probably an error as well.
    (return-from get-one-input (values nil nil)))

  (when (null (car object-list))
    ;; This is probably an error as well.
    (return-from get-one-input (values nil nil)))
  ;;    (error "~&get-one-input called with list having null as first element.~%"))
  
  (let ((head-of-object-list (car object-list))
	(rest-of-object-list (cdr object-list)))

    ;; Take the list apart and try to return something that can be an input.
    (cond
      ;; See if we have a nuumber, an infix expression, or a procedure call.
      ((atom head-of-object-list)
       (if (numberp head-of-object-list)
	   (progn
	     (dformat "number~%")
	     (if (infix-p (car rest-of-object-list))
		 ;; Infix operation.  Swallow up the infix part of the
		 ;; list, return it as the input.
		 (return-from get-one-input (process-infix head-of-object-list rest-of-object-list))
		 ;; Return the number.
		 (return-from get-one-input (values head-of-object-list rest-of-object-list))))
	   (progn
	     ;; Not a number. Should be a procedure call.
	     (let ((proc (find-procedure-from-symbol head-of-object-list)))
	       (when proc
		 (dformat "~&get-one-input: processing ~A~%" head-of-object-list)
		 (if (command-p proc)
		     ;; A command can't be an input to something else. Punt.
		     (return-from get-one-input (values nil object-list))
		     (let ((proc-inputs nil)
			   (rest rest-of-object-list))
		       (dotimes (i (logo-procedure-input-count proc))
			 (dformat "~&get-one-input: getting ~A inputs for ~A~%" (logo-procedure-input-count proc) head-of-object-list)
			 (multiple-value-bind (an-input the-rest)
			     (get-one-input rest)
			   (when an-input
			     (push an-input proc-inputs))
			   (setf rest the-rest)))
		       (let ((proc-obj
			       (if proc-inputs
				   (append (cl:list (logo-procedure-procedure proc)) (nreverse proc-inputs))
				   (cl:list (logo-procedure-procedure proc)))))
			 (if (infix-p (car rest))
			     (return-from get-one-input (process-infix proc-obj rest))
			     (return-from get-one-input (values proc-obj rest)))))))))))

      ;; Something that's quoted (word or quoted list). It can be an
      ;; input. Return it.
      ((quoted-p head-of-object-list)
       (dformat "~&Quoted thing ~A~%" head-of-object-list)
       (values head-of-object-list rest-of-object-list))

      ;; This is a parenthesized item.  It could be an expression.
      ((consp head-of-object-list)
       (dformat "list ~A~%" head-of-object-list)

       (cond
	 ;; Parenthesized infix operations 
	 ((infix-p (car rest-of-object-list))
	  (dformat "list, infix car-rest ~A~%" rest-of-object-list)
	  ;; The whole instruction is something like
	  ;; (3 + 4) * (5 + 6) or (thing 'a) + 5 etc.
	  (multiple-value-bind (infix-expr rest-of-input)
	      (process-infix head-of-object-list rest-of-object-list)
	    (values infix-expr rest-of-input)))

	 (t
	  ;; List, but not infix. This might be a procedure call with
	  ;; a list of inputs.
	  (dformat "list not infix; current object: ~A~%" head-of-object-list)
	  (let ((proc (find-procedure-from-symbol (car head-of-object-list))))
	    (if proc
		;; We have a procedure call, like (sum 1 3 :foo heading).
		;; Need to scan the inputs and make sure they are all
		;; appropriate lisp forms.
		(let ((result (do ((items (cdr head-of-object-list))
				   (input nil)
				   (inputs nil))
				  ((null items) (nreverse inputs))
				(multiple-value-setq (input items) (get-one-input items))
				(push input inputs))))
		  (values (push (car head-of-object-list) result) rest-of-object-list))
		;; Not a procedure call.
		(error 'logo-undefined-procedure :name (car head-of-object-list))))))))))



(defun collect-all-instructions (&optional (stream *standard-input*))
  "This is the main interface to the translator.  Read a logo form.
Translate the logo form into one or more logo instructions.  If there
is more than one, put them in a progn form."
  (let ((*standard-input* stream)
	(*package* (find-package "LOGO")))
    (collect-instructions (read-form))))

(defun collect-instructions (from-here)
  "Given a list of instructions, translate it into one or more logo
instructions.  If there is more than one, put them in a progn form."
  (dformat "~&Collecting instructions: ~A~%" from-here)
  (do ((instruction-list from-here)
       (instructions nil)
       (instruction nil)
       (count 0 (1+ count)))
      ((null instruction-list)
       (if (> count 1)
	   `(progn ,@(nreverse instructions))
	   (car instructions)))
    (dformat "~&Instruction list: ~A; length ~A~%" instruction-list (length instruction-list))
    (multiple-value-setq (instruction instruction-list)
      (translate-instruction-list instruction-list))
    (when instruction (push instruction instructions))))


;;;
;;; Translate particular forms. These need special handling for one
;;; reason or another.
;;;

(defun translate-special-form (instruction-line)
  "Translate a special form. A special form has some issue, like perhaps
needing to bypass the reader, or taking the whole instruction line
with specific inputs. Note that these are still strings, not lists."
  (cond 
    ;; TO takes the name of the procedure and a list of inputs.
    ((to-form instruction-line)
     (setf *procedure-text* instruction-line)
     (translate-to-form (subseq instruction-line (length "to "))))

    ;; LOCAL takes a line of words.
    ((local-form instruction-line)
     (translate-local-form (subseq instruction-line (length "local "))))

    ;; LOCALMAKE takes a word and an input.
    ((localmake-form instruction-line)
     (translate-localmake-form (subseq instruction-line (length "localmake "))))

    ;; The LOAD command needs access to the raw text because it's
    ;; a case-sensitive file name (at least on Unix).
    ((load-form instruction-line)
     ;; XXX This isn't bullet-proof.
     (translate-load-form (subseq instruction-line (1+ (length "load "))))) ; Account for `"' before pathname.

    ((stop-form instruction-line)
     (translate-stop-form instruction-line))

    ((output-form instruction-line)
     (translate-output-form instruction-line))
    (t (error "Unknown special form."))))

;; Just do the load. We need to get the filename from the instruction
;; line with its case preserved, which is why we make this a special
;; form. 
;;;
;;; XXX what happens when this is inside a "to" form? I don't think
;;; this will work because of case issues.
(defun translate-load-form (filename)
  (load filename))

(defun translate-stop-form (instruction-line)
  (unless (defining-procedure)
    (error 'logo-invalid-usage :argument 'stop))
  ;; When we see a stop form, we know the procedure is a command.
  (setf (logo-procedure-kind (find-procedure-from-name *current-procedure*)) :command)
  (parse-instructions instruction-line))

(defun translate-output-form (instruction-line)
  (unless (defining-procedure)
    (error 'logo-invalid-usage :argument 'output))
  ;; When we see an output form, we know the procedure is an operator.
  (setf (logo-procedure-kind (find-procedure-from-name *current-procedure*)) :operator)
  (parse-instructions instruction-line))

(defun translate-to-form (instruction-line)
  "Emit code to define a procedure. The instruction line contains the
name of the procedure and its inputs (if any). (The 'to' was already
stripped off.) The inputs have to be treated specially because they
are prefixed by colons so we have to get to them before the reader
does, since the reader will turn them into calls to `thing'."
  (setf *defining-procedure* t)
  (setf *instruction* nil *remainder* nil)
  (let ((*package* (find-package "LOGO"))
	(line-length (length instruction-line)))
    (multiple-value-bind (function-symbol input-start)
	(read-from-string instruction-line nil nil)
      ;; Make sure we don't have a `to' inside a `to'.  Wait until
      ;; here to check for this so we can get access to the name of
      ;; the procedure we're defining. XXX
      (let* ((real-name
	       (string-trim *whitespace-bag*
			    (subseq instruction-line 0 input-start)))
	     (logo-name (string-upcase real-name))
	     (logo-procedure (find-procedure logo-name)))
	(when logo-procedure
	  (when (logo-procedure-built-in logo-procedure)
	    (format *error-output*
		    "You cannot redefine the built-in Logo procedure ~A.~%
Please try a different name.~%" 
		    logo-name)
	    (return-from translate-to-form))
	  (when (logo-procedure-buried logo-procedure)
	    (format *error-output* 
		    "You cannot redefine the buried procedure ~A.~%
Please unbury it first.~%" 
		    logo-name)
	    (return-from translate-to-form)))
	(setf *current-procedure* real-name)
	(let* ((inputs
		 (if (= input-start line-length) ; No inputs---we're at the end of the line.
		     nil
		     (collect-inputs (subseq instruction-line input-start))))
	       ;; Make dummy procedure so this procedure will be defined in its own body.
	       (procedure
		 (def-proc
		     logo-name
		   real-name
		   function-symbol
		   (length inputs)
		   :kind :command
		   :buried nil))
	       (instructions (collect-to-instructions)))
	  (setf (logo-procedure-text procedure) *procedure-text*)
	  (setf *current-procedure* nil)
	  `(to ,function-symbol ,inputs ,instructions))))))

(defun collect-items (line)
  "Collect items from LINE."
  (do ((*package* (find-package "LOGO"))
       (input-list nil)
       (next 0)
       (object t))
      ((null object) (nreverse input-list))
    (multiple-value-setq (object next)
      (read-from-string line nil nil :start next))
    (when object
      (push object input-list))))


(defun translate-local-form (instruction-line)
  (dformat "LOCAL: Instruction line is ~A~%" instruction-line)
  (unless (defining-procedure)
    (error 'logo-invalid-usage :argument 'local))
  (let* ((locals (collect-items instruction-line))
	 (bad-input (some #'(lambda (input)
			      (and (not (quoted-p input))
				   input))
			  locals)))
    (if bad-input
	(error 'logo-type-error
	       :procedure-name 'local
	       :datum (if (and (consp bad-input) (eq (car bad-input) 'thing))
			  (format nil ":~A" (unquote (cadr bad-input)))
			  (format nil "~A" bad-input))
	       :expected-type (format nil 
				      "sequence of one or more words.~%
(This happened while defining ~A)"
				      (string-upcase *current-procedure*))))
    `(local ,@locals)))


(defun translate-localmake-form (instruction-line)
  (dformat "LOCALMAKE: Instruction line is ~A~%" instruction-line)
  (unless (defining-procedure)
    (error 'logo-invalid-usage :argument 'localmake))
  (multiple-value-bind (variable-name next) (read-from-string instruction-line)
    (unless (quoted-p variable-name)
      (error 'logo-type-error 
	     :procedure-name 'localmake
	     :datum (if (and (consp variable-name) (eq (car variable-name) 'thing))
			(format nil ":~A" (unquote (cadr variable-name)))
			(format nil "~A" variable-name))
	     :expected-type (format nil 
				    "word.~%(This happened while defining ~A)"
				    (string-upcase *current-procedure*))))
    (let ((value (get-one-input (parse-instructions (subseq instruction-line next)))))
      `(localmake ,variable-name ,value))))


(defun collect-inputs (line)
  "Collect the input variables from the procedure definition line after
the `to' and the procedure name have been read. This differs
from COLLECT-ITEMS because it looks for the #\: characters that
indicate variables."
  (do ((*package* (find-package "LOGO"))
       (input-list nil)
       (element nil)
       (position 0 (cl:position #\: line :start position)))
      ((null position) (nreverse input-list))
    (incf position)  ; Move past colon.
    (setf element
	  ;; This nonsense is necessary to get the symbol into the
	  ;; right package.
	  (make-logo-symbol
	   (symbol-name
	    (read-from-string line nil nil :start position))))
    (push element input-list)))

;;; At least the name makes more sense than pr-i.
(defun take-next-byte ()
  (multiple-value-setq (*instruction* *remainder*)
    (translate-instruction-list (or *remainder* (read-form)))))

(defun collect-to-instructions ()
  "Collect a list of logo instructions for the `to' form."
  (let ((*prompt-char* #\>))
    (do ((instruction-list nil)
	 (instruction (take-next-byte) (take-next-byte)))
	((end-of-definition instruction)
	 ;; Use the `end' call as convenient cleanup function.
	 (push instruction instruction-list)
	 (setf *current-procedure* nil)
	 (nreverse instruction-list))
      (dformat "~&***~A is ~A***~%" (car instruction) (type-of (car instruction)))
      (when (defining-procedure)	; Defining something
	(when (eq (car instruction) 'to)
          (error 'logo-defining-error :argument (cadr instruction)))
	(when instruction
          (push instruction instruction-list))))))


(defun end-of-definition (instructions)
  "See if we got a parsed line that starts with `end'."
  (eq (car instructions) 'logo::END))


;;;
;;; For debugging.
;;;
(defun .translate (&optional (stream *standard-input*))
  "Convienient interface to test translator. Also used by DISASSEMBLE
utility."
  (init-logo)     ; Mainly to set up the read table.
  (unwind-protect
      (collect-all-instructions stream)
    (setf *readtable* (copy-readtable nil))))


(defun trace-all ()
  (trace quoted-p unquote translate-instruction-list
	    find-procedure-from-name find-procedure-from-symbol
	    translate-procedure-call
	    get-one-input
	    collect-all-instructions collect-instructions))

(defun untrace-all ()
  (untrace quoted-p unquote translate-instruction-list
	    find-procedure-from-name find-procedure-from-symbol
	    translate-procedure-call
	    get-one-input
	    collect-all-instructions collect-instructions))
