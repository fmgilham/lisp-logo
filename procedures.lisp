;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-31 08:48:50 fred>
;;; Logo-to-lisp translator: procedures.lisp
;;; This file implements many logo procedures.
;;; Original: FMG 8-AUG-1999
;;;
;;; TODO: Implement MORE logo procedures. Implement all the procedures
;;;       in UCBLogo.
;;;
;;;
;;; Note: to avoid name clashes, many common lisp functions are
;;; prefixed with the package, i.e. cl:foo.
;;;

(in-package "LOGO")

(declaim #.*compile-options*)


;;;
;;; Misc system stuff
;;;

(defun bye ()
  (format *query-io* "~&So long and thank you for playing.~%")
  (throw 'logo-exit nil)
;  (format *query-io* "~&Lisping instead of leaving.~%")
  ;; for production actually quit
  ;;  #+sbcl (sb-ext:quit :recklessly-p nil)
  #+cmu (ext:quit)
  ;; for development do this
;  (throw 'logo-non-fatal nil)
  )

(defun wait (time)
  (unless (typep time '(or (single-float 0.0) (double-float 0.0d0) (rational 0)))
    (error 'logo-type-error :procedure-name 'wait :datum time :expected-type "a non-negative number"))
  (force-output *current-stream*)
  (when *display* (xlib:display-force-output *display*))
  (sleep (/ time 60)))

(defun date ()
 (multiple-value-list (get-decoded-time)))

(defun pretty-date ()
  #+sbcl (sb-int:format-universal-time nil (get-universal-time))
  #+cmu (ext:format-universal-time nil (get-universal-time))
  #+ccl (inspector::universal-time-string (get-universal-time)) ;; Ugh...
  #-(or sbcl cmu ccl) broken ;; fixme
)


(defun version ()
  (format *query-io* "~&This is Lisp Logo version ~A~%" cl-user::*logo-version*)
  (format *query-io* "Lisp is ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format *query-io* "Running on ~A~%" (machine-instance))
  (format *query-io* "Hardware is ~A running ~A~%" (machine-version) (uiop/os:operating-system)))



;;;
;;; Load and other i/o-ish things.
;;;

(defun load (thing)
  (let ((*package* (find-package "LOGO"))
	(*interactive* nil))
    (with-logo-error-trapping
      (let* ((filename thing)
	     (pathname (parse-namestring filename))
	     (truename (probe-file filename)))
	(unless truename
	  ;; See if we have a .logo file.
	  (setf truename (probe-file (make-pathname :type "logo" :defaults pathname))))
	(unless truename
	  ;; See if we have a .lgo file.
	  (setf truename (probe-file (make-pathname :type "lgo" :defaults pathname))))
	(unless truename
	  ;; Nope, no file
	  (error 'logo-file-error :pathname filename))
	(format *query-io* "~&Loading ~A~%" truename)
	(force-output *query-io*)
	(flet ((handle-eof (condition)
		 (declare (ignore condition))
		 (format *query-io* "Done.~%")
		 (force-output *query-io*)
		 (throw 'catch-eof nil)))
	  (catch 'catch-eof
	    (with-open-file (f truename)
	      (loop
		(handler-bind ((end-of-file #'handle-eof))
		  (let ((instructions (collect-all-instructions f)))
		    (when instructions
		      (dformat "Evaluating ~A~%" instructions)
		      (logo-eval instructions))
		    nil)))))))))
  (init-logo)
  (values))


(defun logo-read-word ()
  ;; XXX what stream should this read from? *current-stream* doesn't
  ;; work under slime.
  ;; Note: we don't try to deal with any extended number syntax.
  (let* ((line (cl:read-line *query-io* nil nil))
	 (non-num (position-if-not #'digit-char-p line)))
    (if (or (null non-num)
	    ;; Is there a period (decimal point)?
	    (and (char= (aref line non-num) #\.)
		 ;; Is there more than one?
		 (not (position-if-not #'digit-char-p (subseq line (1+ non-num))))))
	;; It's a number
	(read-from-string line nil nil)
	;; It's not a well-formed number
	(make-logo-symbol (string-upcase line)))))


;;
;; CMU has the Hemlock editor.
#+cmu
(defun logo-edit (thing)
  (let ((*package* (find-package "LOGO"))
	(*interactive* nil))
    (with-logo-error-trapping
      (let* ((filename thing)
	     (pathname (parse-namestring filename))
	     (truename (probe-file filename)))
	(unless truename
	  ;; See if we have a .logo file.
	  (setf truename (probe-file (make-pathname :type "logo" :defaults pathname))))
	(unless truename
	  ;; See if we have a .lgo file.
	  (setf truename (probe-file (make-pathname :type "lgo" :defaults pathname))))
	(unless truename
	  ;; Nope, no file
	  (error 'logo-file-error :pathname filename))
	(cl:ed truename))))
  (init-logo)
  (values))

#+cmu
(defun logo-edit-proc (thing)
  (let ((*package* (find-package "LOGO"))
	(*interactive* nil)
	(procedure (find-procedure thing))
	(tmp-file-name (format nil "/tmp/logoedit-~A" (string-downcase (format nil "~A" thing)))))
    (with-open-file (f tmp-file-name :direction :output :if-does-not-exist :create :if-exists :supersede)
      (when procedure
	(let ((proc-text (logo-procedure-text procedure)))
	  (princ proc-text f)
	  (terpri f))))
    (ed tmp-file-name))
  (init-logo)
  (values))



;;;
;;; Print and friends
;;;
;;; print and show are the same except in how they handle top-level lists.
;;;
;;; print [a b c] ==> a b c
;;; show [a b c] ==> [a b c]
;;; print [a b [c d] e f] ==> a b [c d] e f
;;; show [a b [c d] e f] ==> [a b [c d] e f]
;;;

(defun unquote-list (list)
  (mapcar #'unquote list))

(defun print-item (item &optional last (stream *current-stream*))
  (when item
    (if (cl:listp item)
	(dolist (it item)
	  (print-aux item last stream))
	(if last
	    (format stream "~A" item)
	    (format stream "~A " item)))))

(defun print-list-item (item last stream)
  (if (consp item)
      (progn
	(format stream "[")
	(print-aux item last stream)
	(format stream (if last "]" "] ")))
      (print-aux item last stream)))
      
  
(defun print-aux (input last stream)
  (cond ((atom (unquote input))
	 (print-item (unquote input) last stream))
	((eq (wordp input) 'true)
	 (print-item (unquote input) last stream))
	(t				; A list
	 (let* ((in (unquote-list input))
		(butlast (cl:butlast in))
		(last (car (cl:last in))))
	   (dolist (item butlast)
	     (print-list-item item nil stream))
	   (print-list-item last t stream)))))

(defun print (input &rest inputs)
  (let ((i (unquote input)))
    (if inputs
	(let ((ii (unquote-list inputs)))
	  (print-aux i t *current-stream*)
	  (dolist (more (butlast ii))
	    (princ " " *current-stream*)
	    (print-aux more t *current-stream*))
	  (princ " " *current-stream*)
	  (print-aux (last ii) t *current-stream*))
	(print-aux i t *current-stream*)))
  (terpri *current-stream*)
  (values))

(defun show (input &rest inputs)
  (let ((i (unquote input)))
    (if inputs
	(let ((ii (unquote-list inputs)))
	  (print-list-item i t *current-stream*)
	  (dolist (more (butlast ii))
	    (princ " " *current-stream*)
	    (print-list-item more t *current-stream*))
	  (princ " " *current-stream*)
	  (print-list-item (last ii) t *current-stream*))
	(print-list-item i t *current-stream*)))
  (terpri *current-stream*)
  (values))

(defun trace-print ()
  (trace show print print-aux print-list-item print-item unquote-list))


(defun printout-helper (thing)
  (let ((proc (find-procedure-from-symbol thing)))
    (unless proc
      (error 'logo-undefined-procedure :name thing))
    (cl:format t "~&~A~%~%" (logo-procedure-text proc))))

(defun printout (list)
  (dolist (item list)
    (printout-helper item))
  (values))



;;;
;;; System functions
;;;
(defun bury-unbury-helper (thing action)
  (let ((proc (find-procedure-from-symbol thing)))
    (unless proc
      (error 'logo-undefined-procedure :name thing))
    (cond ((eq action :bury)
	   (setf (logo-procedure-buried proc) t))
	  ((eq action :unbury)
	   (setf (logo-procedure-buried proc) nil)))))

	  
(defun bury (list)
  (dolist (item list)
    (bury-unbury-helper item :bury))
  (values))

(defun unbury (list)
  (dolist (item list)
    (bury-unbury-helper item :unbury))
  (values))


;;;
;;; Sequences
;;;

(defun first-number (number)
  (declare (cl:number number))
  (let* ((string (format nil "~A" number))
	 (head (subseq string 0 1)))
    (parse-integer head)))

(defun second-number (number)
  (declare (cl:number number))
  (let* ((string (format nil "~A" number))
	 (head (subseq string 1 2)))
    (parse-integer head)))



(defun first (thing)
  (etypecase thing
    (cl:list (car (unquote thing)))
    (cl:symbol (let* ((string (symbol-name thing))
		      (head (subseq string 0 1)))
		 (intern head "LOGO")))
    (cl:number (first-number thing))))

(defun second (thing)
  (etypecase thing
    (cl:symbol (let* ((string (symbol-name thing))
		      (head (subseq string 1 2)))
		 (intern head "LOGO")))
    (cl:list (cl:second (unquote thing)))
    (cl:number (second-number thing))))

(defun rest-number (num)
  (declare (cl:number num))
  (let* ((string (format nil "~A" num))
	 (rest (subseq string 1)))
    (read-from-string rest)))

(defun butfirst (thing)
  (typecase thing
    (cl:symbol
     (let* ((string (symbol-name thing))
	    (tail (subseq string 1)))
       (intern tail "LOGO")))
    (cl:cons
     (cdr thing))
    (cl:number
     (rest-number thing))
    (t (error 'logo-type-error :procedure-name 'butfirst :datum thing :expected-type "a word, a list or a number"))))


;; Note that common lisp "last" returns a list, not an item.
(defun last (list)
  (car (cl:last (unquote list))))



;;; XXX should work for numbers. (ugh!) E.g. item 3 4567 should output 6.
(defun item (index thing)
  (unless (typep index '(integer 0))
    (error 'logo-type-error :procedure-name 'item :datum index :expected-type "a non-negative integer"))
  (typecase thing
    (cl:symbol
     (let* ((string (symbol-name thing)))
       (subseq string (1- index) index)))
    (cl:cons
     (nth (1- index) (unquote thing)))
    ((cl:array *)
     (aref thing index))
    (cl:number
     (read-from-string (format nil "~A" (aref (format nil "~A" thing) (1- index)))))
    (t (error 'logo-type-error :procedure-name 'item :datum thing :expected-type "a word, a list or an array"))))

(defun set-item (index thing value)
  (setf (elt thing index) value)
  (values))


(defun word (&rest inputs)
  (when (and (< (cl:length inputs) 2) (not (consp inputs)))
    (error 'logo-not-enough-inputs :procedure-name "word"))
  (let ((output-string ""))
    (dolist (item inputs)
      (if (consp item)
	  (error 'logo-type-error :procedure-name 'word :datum item :expected-type "word")
	  (setf output-string (concatenate 'string output-string (format nil "~A" item)))))
    (make-logo-symbol output-string)))

(defun listp (thing)
  (if (and (quoted-p thing) (cl:listp (cdr thing))) 'true 'false))


(defun list (&rest inputs)
  `',(cl:list* inputs))


(defun emptyp (thing)
  (if (null thing) 'true 'false))

(defun fput (first rest)
  (let ((f (unquote first))
	(r (unquote rest)))
    (if (cl:listp rest)
	`',(cons f r)
	(error 'logo-type-error :procedure-name 'fput :datum rest :expected-type "list"))))

(defun lput (item list)
  (let ((l (unquote list)))
    (if (cl:listp l)
	`',(append l (cl:list item))
    (error 'logo-type-error :procedure-name 'lput :datum l :expected-type "list"))))



(defun pick (thing)
  (typecase thing
    (cl:list (nth (cl:random (cl:length thing)) (unquote thing)))
    (cl:number (let ((numstr (format nil "~A" thing)))
		 (parse-integer 
		  (string
		   (elt numstr
			(cl:random
			 (cl:length numstr)))))))
    (cl:symbol (let ((symstr (symbol-name thing)))
		 (string (elt symstr (cl:random (cl:length symstr))))))
    (cl:string (string
		(elt thing
		     (cl:random
		      (cl:length thing)))))
    (t
     (error 'logo-type-error 
	    :procedure-name 'pick
	    :datum thing 
	    :expected-type "number, word or list"))))

;;; Should be able to reverse all logo things (word, number, list).
(defun reverse (thing)
  (typecase thing
    (cl:list (cons 'quote (nreverse (copy-list thing))))
    (t (error 'logo-type-error 
	      :procedure-name 'reverse
	      :datum thing
	      :expected-type "I'm too tired. Use a list."))))

(defun array (size)
  (if (typep size 'fixnum)
      (cl:make-array size)
      (error 'logo-type-error :procedure-name 'array :datum size :expected-type "a non-negative integer")))


;;; Use general lisp function to find length of something.
(defun count (thing)
  (cl:length thing))


(defun flatten-list (x)
  (declare (optimize (speed 3) (safety 3) (debug 1)))
  (labels ((flatten-helper (x r)   ; 'r' is the stuff to the 'right'.
             (cond ((null x) r)
		   ((eq x 'quote) r)
                   ((atom x)
                    (cons x r))
                   (t (flatten-helper 
		       (car x)
                       (flatten-helper (cdr x) r))))))
    (flatten-helper x nil)))

(defun sentence (&rest inputs)
  `',(flatten-list inputs))


;;;
;;; Simple math functions. We have list versions and "spread"
;;; versions. Helps with infix parsing.
;;;
(defun sum (&rest inputs)
  (apply #'cl:+ inputs))

(defun logo-+ (num1 num2)
  (declare (cl:number num1 num2))
  (cl:+ num1 num2))

(defun product (&rest inputs)
  (apply #'cl:* inputs))

(defun logo-* (num1 num2)
  (declare (cl:number num1 num2))
  (cl:* num1 num2))

;; Difference (and -) takes exactly 2 inputs.
(defun difference (num1 num2)
  (declare (cl:number num1 num2))
  (cl:- num1 num2))

(defun logo-- (num1 num2)
  (declare (cl:number num1 num2))
  (cl:- num1 num2))

;; Unary minus. The Lisp reader will parse things like -5, but if you
;; want to negate an arbitrary quantity (like a variable reference)
;; you have to do MINUS :var.
(defun minus (num)
  (declare (cl:number num))
  (cl:- num))

;; Quotient takes 1 or 2 inputs, but only in list form, that is
;; (quotient 2) or (quotient 2 3).
(defun quotient (n1 &optional n2)
  (declare (cl:number n1)
	   (cl:type (or cl:number null) n2))
  (if n2
      (cl:/ n1 n2)
      (cl:/ n1)))

;; / takes exactly two inputs.
(defun logo-/ (n1 n2)
  (declare (cl:number n1 n2))
  (cl:/ n1 n2))


(defun remainder (n1 n2)
  (declare (integer n1 n2))
  (cl:rem n1 n2))

(defun modulo (n1 n2)
  (declare (integer n1 n2))
  (cl:mod n1 n2))

(defun round (n1)
  (declare (cl:number n1))
  (cl:round n1))


;;;
;;; Math functions
;;;
(defun ln (num)
  (declare (cl:number num))
  (cl:log num))

(defun log10 (num)
  (declare (cl:number num))
  (cl:log num 10))

(defun sqrt (num)
  (declare (cl:number num))
  (if (> 0 num)
      (error 'logo-type-error :procedure-name 'sqrt :datum num :expected-type "non-negative number")
      (cl:sqrt num)))
;;
;; When n1 is negative and n2 is non-integer, the result will be
;; complex, but what the heck. If you got to messing around with POWER
;; you will probably be pleasantly surprised to find this out.
(defun power (n1 n2)
  (declare (cl:number n1 n2))
  (expt n1 n2))

(defun logo-^ (n1 n2)
  (declare (cl:number n1 n2))
  (expt n1 n2))
	   


(defun exp (n)
  (declare (cl:number n))
  (cl:exp n))


;;
;; Trigonometric stuff. 
(defun deg-to-rad (deg)
  (declare (cl:number deg))
  (/ (* cl:pi deg) 180.0))

(defun rad-to-deg (rad)
  (declare (cl:number rad))
  (/ (* 180 rad) cl:pi))

(defun radsin (num)
  (declare (cl:number num))
  (cl:sin num))

(defun sin (num)
  (radsin (deg-to-rad num)))

(defun radcos (num)
  (declare (cl:number num))
  (cl:cos num))

(defun cos (num)
  (declare (cl:number num))
  (radcos (deg-to-rad num)))

(defun radarctan (n1 &optional (n2 nil))
  (declare (cl:number n1)
	   (cl:type (or cl:number null) n2))
  (if n2
      (cl:atan n2 n1)
      (cl:atan n1)))

(defun arctan (n1 &optional (n2 nil))
  (declare (cl:number n1)
	   (cl:type (or cl:number null) n2))
  (rad-to-deg (if n2
		  (radarctan n1 n2)
		  (radarctan n1))))

(defun pi ()
  cl:pi)


(defun random (x &optional y)
  (declare (cl:number x))
  (declare (ignore y)) ; XXX I think some versions use y as a minimum.
  (cl:random x))


;;;
;;; All comparison operators and tests return either TRUE or FALSE. 
;;;
;;; All the usual infix operators take two inputs. In lisp the infix
;;; operators take lists and the "eq" family functions take two
;;; inputs. In logo x = y is OK. (= x y z) is not. But (equalp x y z)
;;; is OK.
(defun logo-> (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:> num1 num2)
      'true
      'false))

(defun greaterp (&rest inputs)
  (if (apply #'cl:> inputs)
      'true
      'false))

(defun logo-= (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:= num1 num2)
      'true
      'false))

(defun equalp (&rest inputs)
  (case (length inputs)
    ((0 1) (error 'logo-not-enough-inputs :procedure 'equalp ))
    (2 (if (apply #'equal inputs) 'true 'false))
    (otherwise (error "Too many inputs to equalp."))))


(defun logo-< (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:< num1 num2)
      'true
      'false))

(defun lessp (&rest inputs)
  (if (apply #'cl:< inputs)
      'true
      'false))

(defun logo->= (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:>= num1 num2)
      'true
      'false))

(defun logo-<= (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:<= num1 num2)
      'true
      'false))

(defun geqp (&rest inputs)
  (if (apply #'cl:>= inputs)
      'true
      'false))

(defun leqp (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:<= num1 num2)
      'true
      'false))

(defun logo-<> (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:/= num1 num2)
      'true
      'false))

(defun neqp (num1 num2)
  (declare (cl:number num1 num2))
  (if (cl:/= num1 num2)
      'true
      'false))


;;;
;;; Should MAKE be a function or a macro?
;;; The PROCLAIM is necessary to avoid uncouth warnings by lisp.
;;; Can also implement MAKE using the translator and a macro.
(defun make (word value)
  (proclaim  '(cl:special word))
  (setf (symbol-value word) value)
  (values))

#-(and)
(defmacro make (word value)
  `(set ,(unquote word) ,value))


;;; If a name names a binding, it's a variable.
(defun namep (thing)
  "Is THING the name of a variable?"
  (if (and (symbolp thing) (boundp thing))
      'true
      'false))

(defun wordp (thing)
  "Is THING a word? Note that numbers are words."
  (if (or (symbolp thing) (numberp thing))
      'true
      'false))


;;;
;;; Runlists.
;;;

(defun ref-place (obj)
  "Variable reference; assumes obj is :foo or (thing (quote foo))."
  (unquote (cl:cadr obj)))

;;; Get down to the bare metal.
(defun runlist-expand (runlist)
  (if (thing-p runlist)
      ;; Keep unwinding the `thing' forms until we find
      ;; the thing referenced, then look it up.
      (unquote (runlist-expand (ref-place runlist)))
      ;; Simple case---just strip the quote.
      (unquote runlist)))


;;; In the following, Try to get the compiler as fast as possible.
;;; These runlists are likely to be little pieces of code called often
;;; --- and so compiled often.
#+sbcl
(defun compile-expand (runlist)
  "Compile the runlist, calling BUILD-RUNLIST first."
  (let (#-(and) (sb-ext:*evaluator-mode* :interpret) ; Doesn't seem to make a difference.
	(rl (build-runlist runlist)))
    (sb-ext:without-package-locks 
      (compile nil
	       `(lambda ()
		  (declare (optimize (compilation-speed 3) (space 0) (safety 0) (debug 1) (sb-ext:inhibit-warnings 3)))
		  ,rl)))))

#+cmu
(defun compile-expand (runlist)
  "Compile the runlist, calling BUILD-RUNLIST first."
  (let* ((cl:*compile-print* nil)
	 (rl (build-runlist runlist)))
    (compile nil `(lambda ()
		    (declare (optimize (compilation-speed 3) (space 0) (safety 0) (ext:inhibit-warnings 3)))
		    ,rl))))

#+ccl
(defun compile-expand (runlist)
  "Compile the runlist, calling BUILD-RUNLIST first."
  (let ((rl (build-runlist runlist)))
    (compile nil `(lambda ()
		    (declare (optimize (compilation-speed 3) (space 0) (safety 0)))
		    ,rl))))


;;; Runtime evaluation of a list. The list gets compiled first, then
;;; executed, all at runtime. This is the whole point of the RUN
;;; macro---allowing code to build code that then gets compiled and
;;; executed.
(defmacro run (executable-thing)
  `(progn
     (cond ((atom ,executable-thing)
	    (unquote ,executable-thing))
	   ;; Consp because NIL is a list.
	   ((consp ,executable-thing)
	    (funcall (compile-expand ,executable-thing)))
	   (t
	    (error 'logo-type-error :procedure-name "run"
				    :datum ,executable-thing 
				    :expected-type "atom, variable, or list of instructions"))
	   )))


;; Called by macros that use runlists.
(defun build-runlist (runlist)
  (dformat "~&Build-runlist: runlist ~A~%" runlist)
  (dformat "~&Build-runlist: defining-procedure is ~A~%" (defining-procedure))
  ;; Make sure all the abbreviations to built-in procedure names are
  ;; translated. Since it's a quoted list, that won't have been done
  ;; by the reader.
  (collect-instructions (parse-thing (runlist-expand runlist))))



;;;
;;; Iteration
;;;
(defvar repcount -1)

;;; Repeat the runlist COUNT times. Note use of gensym to avoid
;;; capture of REPCOUNT in nested usage.
(defmacro repeat (count runlist)
  (let ((rl (build-runlist runlist))
	(endcount (gensym)))
    `(let ((repcount 0)
	   (,endcount ,count))
       (declare (cl:special repcount))
       (loop
	 #+(and cmu mp) (mp:process-yield) ; Allow this code to be managed by the system.
	 (incf repcount)
	 (when (> repcount ,endcount)
	   (return (values)))
	 ,rl))))
	 


;;; Never exits unless something inside does "stop" or "output" or
;;; "throw".
(defmacro forever (runlist)
  (let ((rl (build-runlist runlist)))
    `(let ((repcount 0))
       (declare (cl:special repcount))
       (loop
	 #+(and cmu mp) (mp:process-yield) ; Allow this code to be managed by the system.
	 (incf repcount)
	 ,rl))))


;; Give access to the iteration variable.
(defmacro repcount ()
  `(symbol-value 'repcount))


(defmacro while (cond-exp runlist)
  (let ((rl (build-runlist runlist)))
    `(do ()
	 ((eq ,cond-exp 'false))
       #+(and cmu mp) (mp:process-yield) ; Allow this code to be managed by the system.
       ,rl)))

(defmacro until (cond-exp runlist)
  (let ((rl (build-runlist runlist)))
    `(do ()
	 ((eq ,cond-exp 'true))
       #+(and cmu mp) (mp:process-yield) ; Allow this code to be managed by the system.
       ,rl)))


#||
;;;
;;; Broken

(defun istrue (exp logical-func)
  (let* ((unquoted-exp (cl:print (unquote exp)))
	 (e (if (cl:listp unquoted-exp) (run unquoted-exp) unquoted-exp)))
    (cond ((eq e 'true) t)
 	  ((eq e 'false) nil)
	  (t (error 'logo-type-error :procedure-name logical-func :datum exp :expected-type "boolean (\"true or \"false)")))))

;; XXX These don't really work right.
;; XXX They should take lists.
(defmacro logo-and (&rest boolean-exps)
  `(let ((truth (cl:every #'(lambda (thing) (istrue thing 'and)) ',boolean-exps)))
     (cl:if truth 'true 'false)))

;; XXX
(defmacro logo-or (&rest boolean-exps)
  `(let ((truth (cl:some #'(lambda (thing) (istrue thing 'or)) ',boolean-exps)))
     (cl:if truth 'true 'false)))
||#


;; XXX These don't really work right.
(defmacro logo-and (input1 input2)
  `(if (eq ,input1 'true)
	  (if (eq ,input2 'true)
	      'true
	      'false)
	  'false))

;; XXX
(defmacro logo-or (input1 input2)
  `(if (eq ,input1 'true)
	  'true
	  (if (eq ,input2 'true)
	  'true
	  'false)))

;;;
;;; Takes one input. If a list, runs the list. The list must produce
;;; either "true or "false. If a word, the word must be either "true
;;; or "false. Returns the negation ("true for "false and vice versa).
(defun logo-not (input)
  (let ((result
	  (if (cl:listp input)
	      (run input)
	      input)))
    (unless (or (eq result 'true) (eq result 'false))
      (error 'logo-type-error :procedure-name 'not :datum input :expected-type "boolean or boolean expression"))
    (if (eq result 'true) 'false 'true)))


(defmacro logo-ifelse (test to-do-1 to-do-2)
  (let ((rl1 (build-runlist to-do-1))
	(rl2 (build-runlist to-do-2)))
    `(cond ((eq ,test 'true) ,rl1)
	   ((eq ,test 'false) ,rl2)
	   (t (error 'logo-type-error :procedure-name 'ifelse :datum ,test :expected-type "boolean (true or false)")))))

(defmacro logo-if (test to-do)
  (let ((rl (build-runlist to-do)))
    `(cond ((eq ,test 'true) ,rl)
	   ((eq ,test 'false) (values))
	   (t (error 'logo-type-error :procedure-name "if" :datum ,test :expected-type "boolean (true or false)")))))

;;;
;;; TEST, IFTRUE, IFFALSE
(defvar test-result)

(defun logo-test (bool)
  (if (or (eq bool 'true) (eq bool 'false))
      (progn (setf test-result bool) (values))
      (error 'logo-type-error :procedure-name 'test :datum bool :expected-type "boolean (true or false)")))

(defmacro logo-if-true (to-do)
  (let ((rl (build-runlist to-do)))
    `(if (eq test-result 'true) ,rl (values))))

(defmacro logo-if-false (to-do)
  (let ((rl (build-runlist to-do)))
    `(if (eq test-result 'false) ,rl (values))))


;;;
;;; Expand to-form bodies. Used by the TO macro.
;;;

;;;
;;;
;;; When LOCAL or LOCALMAKE is encountered, create a new dynamic scope.
;;;
;;; LOCAL should give
;;;
;;;      (let (var1 var2 var3 ...)
;;;        (declare (special var1 var2 var3 ...))
;;;	   (body))
;;;
;;; LOCALMAKE should give
;;;
;;;      (let ((var1 val1))
;;;        (declare (special var1))
;;;        (body))
;;;
;;; Note that there may be stuff before the LOCAL forms. This handles
;;; that properly.

(defun expand-local-body (to-body)
  (do* ((to-forms to-body (cdr to-forms))
	(form (car to-forms) (car to-forms)))
       ((eq (car form) 'end))

    (dformat "Expand-local-body form: ~A~%" form)

    ;; If we are here, we know we got a scope-inducing form. (If
    ;; somehow not, it's an error.)
    ;; XXX error checking for both these.
    (ecase (car form)
      (local 
       ;; Add a new scope with name(s) of unbound variable(s).
       (let ((local-names (mapcar #'unquote (cdr form))))
	 (return `(let ,local-names
		    (declare (special ,@local-names))
		    ,@(expand-body (cdr to-forms))))))
      (localmake
       ;; Add a new scope with one binding
       (let ((name (unquote (cadr form)))
	     (value (caddr form)))
	 (return `(let ((,name ,value))
		    (declare (special ,name))
		    ,@(expand-body (cdr to-forms))))))
      )))


;;; Main function to expand the to body.

(defun expand-body (to-body)

  ;; Accumulate forms, checking for "scope-inducing" forms.
  (do* ((to-forms to-body (cdr to-forms))
	(form (car to-forms) (car to-forms))
	(result-body nil))
       ((eq (car form) 'end)
	result-body)

    (dformat "Expand-body form: ~A~%" form)

    (if (or (eq (car form) 'local)
	    (eq (car form) 'localmake))
	;; Add a new scope
	(if result-body
	    (return (append result-body (cl:list (expand-local-body to-forms))))
	    (return (cl:list (expand-local-body to-forms))))
	;; Handle non-scope-inducing forms
	(setf result-body
	      (if result-body
		  (append result-body (cl:list form))
		  (cl:list form))))))


(defun expand-declarations (&optional input-list)
  (let ((declaration-forms '((optimize (speed 2) (safety 3) (debug 3) 
			      #+cmu (ext:inhibit-warnings 3)
			      #+sbcl (sb-ext:inhibit-warnings 3)))))
    (when input-list
      (setf declaration-forms
	    (append declaration-forms
		    (cl:list `(special ,@input-list)))))
    `(declare ,@declaration-forms)))

;;;
;;; This does logo procedure definition by expanding a lisp "to" macro
;;; call (provided by the logo reader/translator) into a defun call
;;; and compiling the procedure. It expects that the procedure
;;; structure has already been created during the translation process.
;;;
;;; For SBCL we disable package locks and muffle compiler warnings.
;;; Similar things could be done for other compilers.
(defmacro to (name input-list body)
  (let* ((proc (find-procedure-from-symbol name))
	 (proc-name (logo-procedure-procedure proc))
	 (declarations (expand-declarations input-list))
	 (to-body (expand-body body))
	 )
    `(progn
       (setf (logo-procedure-input-count ,proc) (length ',input-list))
       (format *debug-io* "Defining procedure named ~A~%" ',proc-name)
       (defun ,proc-name ,input-list
	 ,declarations
	 ,@to-body
	 )
       ;; Something strange about CMU lisp; it outputs a "compilation
       ;; unit aborted" message. Rumors were that the following would
       ;; fix it, but it doesn't seem to help.
       #+cmu (setf ,proc-name (function ,proc-name))
       (format *debug-io* "Compiling procedure ~A~%" ',proc-name)
       (force-output *debug-io*)
       (#+sbcl sb-ext:without-package-locks #-sbcl progn
	(compile ',proc-name)
	(format *debug-io* "~A defined.~%" ',proc-name))
       nil)))


;;;
;;; These macros splice a "return-from <procedure-name>" into the body
;;; of the defun, ether returning a value or returning no values. When
;;; called from the toplevel they throw an error.
;;;

;;; Function-end cleanup.
;;; XXX Actually this looks like it will never be expanded. See
;;; EXPAND-BODY above.
(defmacro end ()
  (if (defining-procedure)
      (prog1
	  `(return-from ,*current-procedure* (values))
	(setf *current-procedure* nil))
      `(throw 'not-in-procedure 'end)))

;;; Operators return something.
(defmacro output (thing)
  (if (defining-procedure)
      (let ((proc (find-procedure-from-symbol *current-procedure*)))
	(setf (logo-procedure-kind proc) :operator)
	`(return-from ,*current-procedure* ,thing))
      `(throw 'not-in-procedure 'output)))

;;; Commands don't return anything.
(defmacro stop ()
  (if (defining-procedure)
      (let ((proc (find-procedure-from-symbol *current-procedure*)))
	(setf (logo-procedure-kind proc) :command)
	`(return-from ,*current-procedure* (values)))
      `(throw 'not-in-procedure 'stop)))


;;; LOCALMAKE (and LOCAL) should not be invoked at the toplevel. If
;;; they are in procedure bodies they get expanded before they are
;;; seen by the toplevel loop. Because LOCAL takes a varying number of
;;; inputs, it is sufficiently complicated that it gets handled
;;; elsewhere.
(defun localmake (thing1 thing2)
  (declare (ignore thing1 thing2))
  (unless (defining-procedure)
    (error 'logo-invalid-usage :argument 'localmake)))



;;; XXX handle multiple things (i.e. trace [foo bar baz] or some such.
(defmacro logo-trace (thing)
  (let* ((name (unquote thing))
	 (procedure (find-procedure-from-symbol name)))
    (unless procedure
      (error "Can't trace undefined procedure ~A~%" name))
    `(progn (trace ,(logo-procedure-procedure procedure)) (values))))


;;; XXX handle multiple things correctly
(defmacro logo-untrace (thing)
  (if thing
      (let ((name (unquote thing)))
	(if (eq name 'all)
	    `(progn (untrace) (values))
	    `(progn (untrace ,nam) (values))))))


(defun logo-debug (thing)
  (cond ((string-equal (symbol-name thing) "true")
	 (format t "~&Turning debug output *ON*~%")
	 (setf *debug* t))
	((string-equal (symbol-name thing) "false")
	 (format t "~&Turning debug statements *OFF*~%")
	 (setf *debug* nil))
	(t (format t "~&Debugging statments are ~A~%" (if *debug* 'on 'off))))
  (values))


(defun disassemble (procedure)
  (let ((*interactive* nil)
	(logo-procedure (find-procedure-from-symbol procedure)))
    (unless logo-procedure
      (error 'logo-undefined-procedure :name procedure))
    (when (logo-procedure-built-in logo-procedure)
      (error 'logo-type-error :procedure-name "DISASSEMBLE" 
			      :datum procedure 
			      :expected-type "user-defined procedure name"))
    (let* ((cl:*print-escape* nil)
	   (procedure-text (logo-procedure-text logo-procedure))
	   (lisp-code (with-input-from-string (s procedure-text)
		       (.translate s))))
      (pprint (fourth
	       (#+ccl ccl:macroexpand-all
		#+sbcl sb-walker:macroexpand-all
		#+cmu walker:macroexpand-all
		#-(or ccl sbcl cmu) macroexpand-1
		lisp-code)))
      (terpri))))


(defun error-breaks (thing)
  (cond ((string-equal (symbol-name thing) "true")
	 (format t "~&Turning error breaks *ON*~%")
	 (setf *break-on-signals* t))
	((string-equal (symbol-name thing) "false")
	 (format t "~&Turning error breaks *OFF*~%")
	 (setf *break-on-signals* nil))
	(t (format t "~&Breaks are ~A~%" (if cl:*break-on-signals* 'on 'off))))
  (values))

(defun lisp ()
  "Escape to lisp."
  (format *error-output* "~&~%***Type (logo) to return to logo.***~%~%")
  (force-output *error-output*)
  (throw 'logo-non-fatal 'lisp-escape))



;;; def-logo-primitive
;;;      logo name: name by which the procedure is accessed in logo
;;;      function name: the name of the lisp function
;;;      # inputs: the number of inputs the prcedure takes
;;;      kind: operator (outputs something), command (side-effect
;;;            only), or special form (weird handling)
;;;      infix weight: If the procedure is an infix oerator, the
;;;                    precedence. If nil, it's not infix.
;;;      accepts list: can the procedure be called with a, such as
;;;                    (print 1 2 3) or print [1 2 3 4]
;;;
;;;      Note that commands should not return anything, i.e. with the
;;;      (values) form.
;;;
;;;      Logo procedures that clash with CL equivalents are either
;;;      shadowed (see package.lisp) or given a name with "logo-"
;;;      prepended. Also, in lisp code we use CAR, CDR and friends to
;;;      avoid clashing with FIRST and REST and to avoid having to
;;;      remember to prepend cl: to everything (only some things...).
;;;
;;;                 procedure     function     #                 infix  accepts
;;;                 name          name       inputs     kind     weight list

;; Math
(def-logo-primitive "+"                "logo-+"             2     :operator   20     nil)
(def-logo-primitive "-"                "logo--"             2     :operator   20     nil)
(def-logo-primitive "*"                "logo-*"             2     :operator   30     nil)
(def-logo-primitive "/"                "logo-/"             2     :operator   30     nil)
(def-logo-primitive "^"                "logo-^"             2     :operator   40     nil)
(def-logo-primitive "="                "logo-="             2     :operator   10     nil)
(def-logo-primitive ">"                "logo->"             2     :operator   10     nil)
(def-logo-primitive "<"                "logo-<"             2     :operator   10     nil)
(def-logo-primitive ">="               "logo->="            2     :operator   10     nil)
(def-logo-primitive "<="               "logo-<="            2     :operator   10     nil)
(def-logo-primitive "<>"               "logo-<>"            2     :operator   10     nil)
(def-logo-primitive "SUM"              "sum"                2     :operator   nil      t)
(def-logo-primitive "DIFFERENCE"       "difference"         2     :operator   nil    nil)
(def-logo-primitive "MINUS"            "minus"              1     :operator   nil    nil)
(def-logo-primitive "PRODUCT"          "product"            2     :operator   nil      t)
(def-logo-primitive "QUOTIENT"         "quotient"           2     :operator   nil    nil)
(def-logo-primitive "EQUALP"           "equalp"             2     :operator   nil    nil)
(def-logo-primitive "EQUAL?"           "equalp"             2     :operator   nil    nil)
(def-logo-primitive "GREATERP"         "greaterp"           2     :operator   nil    nil)
(def-logo-primitive "GREATER?"         "greaterp"           2     :operator   nil    nil)
(def-logo-primitive "GREATEREQUALP"    "geqp"               2     :operator   nil    nil)
(def-logo-primitive "GREATEREQUAL?"    "geqp"               2     :operator   nil    nil)
(def-logo-primitive "LESSP"            "lessp"              2     :operator   nil    nil)
(def-logo-primitive "LESS?"            "lessp"              2     :operator   nil    nil)
(def-logo-primitive "LESSEQUALP"       "leqp"               2     :operator   nil    nil)
(def-logo-primitive "LESSEQUAL?"       "leqp"               2     :operator   nil    nil)
(def-logo-primitive "REMAINDER"        "remainder"          2     :operator   nil      t)
(def-logo-primitive "MODULO"           "modulo"             2     :operator   nil    nil)
(def-logo-primitive "ROUND"            "round"              1     :operator   nil    nil)
(def-logo-primitive "SQRT"             "sqrt"               1     :operator   nil    nil)
(def-logo-primitive "LN"               "ln"                 1     :operator   nil    nil)
(def-logo-primitive "LOG10"            "log10"              1     :operator   nil    nil)
(def-logo-primitive "EXP"              "exp"                1     :operator   nil    nil)
(def-logo-primitive "POWER"            "power"              2     :operator   nil    nil)
(def-logo-primitive "SIN"              "sin"                1     :operator   nil    nil)
(def-logo-primitive "RADSIN"           "radsin"             1     :operator   nil    nil)
(def-logo-primitive "COS"              "cos"                1     :operator   nil    nil)
(def-logo-primitive "RADCOS"           "radcos"             1     :operator   nil    nil)
(def-logo-primitive "TAN"              "tan"                1     :operator   nil    nil)
(def-logo-primitive "ARCTAN"           "arctan"             1     :operator   nil      t)
(def-logo-primitive "RADARCTAN"        "radarctan"          1     :operator   nil      t)
(def-logo-primitive "PI"               "pi"                 0     :operator   nil    nil)
(def-logo-primitive "RANDOM"           "random"             1     :operator   nil    nil)


;; Logical operators. Way too much trouble to shadow these and use the
;; cl:<foo> versions everywhere.
(def-logo-primitive "AND"              "logo-and"           2     :operator   nil    nil)
(def-logo-primitive "OR"               "logo-or"            2     :operator   nil    nil)
(def-logo-primitive "NOT"              "logo-not"           1     :operator   nil    nil)


;; Word/List/Array
(def-logo-primitive "BUTFIRST"         "butfirst"           1     :operator   nil    nil)
(def-logo-primitive "BF"               "butfirst"           1     :operator   nil    nil)
(def-logo-primitive "EMPTYP"           "emptyp"             1     :operator   nil    nil)
(def-logo-primitive "EMPTY?"           "emptyp"             1     :operator   nil    nil)
(def-logo-primitive "FIRST"            "first"              1     :operator   nil    nil)
(def-logo-primitive "SECOND"           "second"             1     :operator   nil    nil)
(def-logo-primitive "LAST"             "last"               1     :operator   nil    nil)
(def-logo-primitive "PICK"             "pick"               1     :operator   nil    nil)
(def-logo-primitive "REVERSE"          "reverse"            1     :operator   nil    nil)
(def-logo-primitive "FPUT"             "fput"               2     :operator   nil    nil)
(def-logo-primitive "LPUT"             "lput"               2     :operator   nil    nil)
(def-logo-primitive "ITEM"             "item"               2     :operator   nil    nil)
(def-logo-primitive "LIST"             "list"               2     :operator   nil      t)
(def-logo-primitive "LISTP"            "listp"              1     :operator   nil    nil)
(def-logo-primitive "LIST?"            "listp"              1     :operator   nil    nil)
(def-logo-primitive "SENTENCE"         "sentence"           2     :operator   nil      t)
(def-logo-primitive "SE"               "sentence"           2     :operator   nil      t)
(def-logo-primitive "WORD"             "word"               2     :operator   nil      t)
(def-logo-primitive "ARRAY"            "array"              1     :operator   nil    nil)
(def-logo-primitive "SETITEM"          "set-item"           3     :command    nil    nil)
(def-logo-primitive "COUNT"            "count"              1     :operator   nil    nil)


;; Variables
(def-logo-primitive "LOCAL"            "local"              0     :special    nil      t)
(def-logo-primitive "LOCALMAKE"        "localmake"          2     :command    nil    nil)
(def-logo-primitive "MAKE"             "make"               2     :command    nil    nil)
(def-logo-primitive "THING"            "thing"              1     :operator   nil    nil)
(def-logo-primitive "NAMEP"            "namep"              1     :operator   nil    nil)
(def-logo-primitive "NAME?"            "namep"              1     :operator   nil    nil)
(def-logo-primitive "WORDP"            "wordp"              1     :operator   nil    nil)
(def-logo-primitive "WORD?"            "wordp"              1     :operator   nil    nil)


;; Control
(def-logo-primitive "IF"               "logo-if"            2     :special    nil    nil)
(def-logo-primitive "IFELSE"           "logo-ifelse"        3     :special    nil    nil)
(def-logo-primitive "TEST"             "logo-test"          1     :command    nil    nil)
(def-logo-primitive "IFFALSE"          "logo-if-false"      1     :command    nil    nil)
(def-logo-primitive "IFF"              "logo-if-false"      1     :command    nil    nil)
(def-logo-primitive "IFTRUE"           "logo-if-true"       1     :command    nil    nil)
(def-logo-primitive "IFT"              "logo-if-true"       1     :command    nil    nil)

(def-logo-primitive "REPEAT"           "repeat"             2     :command    nil    nil)
(def-logo-primitive "REPCOUNT"         "repcount"           0     :operator   nil    nil)
(def-logo-primitive "FOREVER"          "forever"            1     :command    nil    nil)
(def-logo-primitive "WHILE"            "while"	            2     :command    nil    nil)
(def-logo-primitive "UNTIL"            "until"	            2     :command    nil    nil)
(def-logo-primitive "RUN"              "run"                1     :special    nil    nil)
(def-logo-primitive "STOP"             "stop"               0     :command    nil    nil)
(def-logo-primitive "OUTPUT"           "output"             1     :command    nil    nil)
(def-logo-primitive "OP"               "output"             1     :command    nil    nil)

;; I/O
(def-logo-primitive "PRINT"            "print"              1     :command    nil      t)
(def-logo-primitive "PR"               "print"              1     :command    nil      t)
(def-logo-primitive "SHOW"             "show"               1     :command    nil      t)
(def-logo-primitive "LOAD"             "load"               1     :command    nil    nil)
(def-logo-primitive "READWORD"         "logo-read-word"     0     :operator   nil    nil)
(def-logo-primitive "RW"               "logo-read-word"     0     :operator   nil    nil)
;;(def-logo-primitive "EDIT"             "logo-edit"          1     :command    nil    nil)
;;(def-logo-primitive "EDITPROC"         "logo-edit-proc"     1     :command    nil    nil)


;; Procedures
(def-logo-primitive "TO"               "to"                 0     :special    nil    nil)
(def-logo-primitive "END"              "end"                0     :command    nil    nil)
(def-logo-primitive "PO"               "printout"           1     :command    nil    nil)
(def-logo-primitive "BURY"             "bury"               1     :command    nil    nil)
(def-logo-primitive "UNBURY"           "unbury"             1     :command    nil    nil)


;; Misc.
(def-logo-primitive "BYE"              "bye"                0     :command    nil    nil)
(def-logo-primitive "WAIT"             "wait"               1     :command    nil    nil)
(def-logo-primitive "LOGOVERSION"      "version"            0     :command    nil    nil)
(def-logo-primitive "DATE"             "date"               0     :operator   nil    nil)
(def-logo-primitive "PRETTYDATE"       "pretty-date"        0     :operator   nil    nil)
;; LOGO-TRACE because I'm tired of having to type CL:TRACE
(def-logo-primitive "TRACE"            "logo-trace"         1     :command    nil      t)
(def-logo-primitive "UNTRACE"          "logo-untrace"       1     :command    nil      t)
(def-logo-primitive "DEBUG"            "logo-debug"         1     :command    nil    nil)
(def-logo-primitive "ERRORBREAK"       "error-breaks"       1     :command    nil    nil)
(def-logo-primitive "DISASSEMBLE"      "disassemble"        1     :command    nil    nil)
(def-logo-primitive "LISP"             "lisp"               0     :command    nil    nil)
