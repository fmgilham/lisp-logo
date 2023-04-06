;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-31 08:44:59 fred>
;;; Logo-to-lisp translator: reader.lisp This file contains the reader
;;; for logo. It takes a line of text (including possibly continuation
;;; lines) and returns a list of lisp objects. All procedure calls
;;; should be translated from logo form to lisp, except if they are in
;;; runlists. Those get handled when the runlist is expanded.
;;; Original: FMG 8-AUG-1999

(in-package "LOGO")

(declaim #.*compile-options*)


;;;
;;; Readtable stuff
;;;
(defun read-quote (stream ignore)
  "Read-macro for logo quote."
  (declare (ignore ignore))
  (let ((*package* (find-package "LOGO")))
    `(cl:quote ,(read stream))))

(defun read-colon (stream ignore)
  "Read-macro for logo colon."
  (declare (ignore ignore))
  (let ((*package* (find-package "LOGO")))
    `(thing (cl:quote ,(read stream t nil t)))))

(defun quoted-p (thing)
  (and (consp thing)
       (eq (car thing) 'cl:quote)))

(defun unquote (thing)
  (if (quoted-p thing)
      (let ((unquoted-thing (cdr thing)))
	(if (= (length unquoted-thing) 1) 
	    (car unquoted-thing)  ; thing was something like "foo
	    unquoted-thing))      ; thing was something like [foo bar baz]
      thing))

(defun read-quoted-list (stream char)
  "Read-macro to read a logo list, delimited by square brackets.
Note: all logo lists [stuff] are quoted. Parenthesized objects are
'grouped' but are not actually logo lists."
  (declare (ignore char))
  `(cl:quote ,(read-delimited-list #\] stream t)))

;;; Unused for the present
(defun read-arithmetic-operator (stream char)
  (declare (ignore stream))
  (case char
    (#\^ (make-logo-symbol "^"))
    (#\+ (make-logo-symbol "+"))
    (#\* (make-logo-symbol "*"))
    (#\- (make-logo-symbol "-"))
    (#\/ (make-logo-symbol "/"))
    (#\= (make-logo-symbol "="))
    (#\< (make-logo-symbol "<"))
    (#\> (make-logo-symbol ">"))
    ))

(defun read-arithmetic-dipthong (stream char)
  (let ((c (peek-char nil stream)))
    (cond ((char= c #\=)
	   (read-char stream) ; Swallow #\=
	   (format nil "~C=" char))
	  ((and (char= char #\<) (char= c #\>))
	   (read-char stream) ; Swallow #\>
	   (make-logo-symbol "<>"))
	  (t
	   (make-logo-symbol (format nil "~C" char))))))

(defun setup-logo-readtable ()
  "Set up the read table so it will translate logo syntax into normal
lisp syntax."

  ;; Cmucl's debugger winds up with the wrong readtable if it gets
  ;; called from inside logo (e.g. interrupts)
  #+cmu (setf debug:*debug-readtable* (copy-readtable nil)) 
  #+sbcl (setf sb-debug:*debug-readtable* (copy-readtable nil)) 

  (setf (readtable-case *logo-readtable*) :UPCASE)
  (setf cl:*readtable* *logo-readtable*)
  (set-macro-character #\" #'read-quote)
  (set-macro-character #\: #'read-colon)
  (set-syntax-from-char #\' #\a)
  (set-syntax-from-char #\. #\a)
  (set-macro-character #\[ #'read-quoted-list)
  (set-macro-character #\] (get-macro-character #\)))

  ;; This is a good idea except ... there is ambiguity with "-". When
  ;; we come across the parse (- <number>) it could have come from
  ;; -<number> or <number>-<number>. It's not worth the trouble to try
  ;; to figure out all the cases, so we'll not do this for now.
  ;;;
  ;; Currently the Lisp reader takes care of reading negative numbers.
  ;; Let's try ^, +, * and /. 
  ;;
  ;; So you can write 3+4, 3*4 or 3/4. But you can't write 3-4 because
  ;; that will get parsed as 3 -4 instead of 3 - 4. So you have to
  ;; write 3 - 4 in the case of subtraction.
  (set-macro-character #\^ #'read-arithmetic-operator)
  (set-macro-character #\+ #'read-arithmetic-operator)
#-(and)  (set-macro-character #\- #'read-arithmetic-operator)
  (set-macro-character #\* #'read-arithmetic-operator)
  (set-macro-character #\/ #'read-arithmetic-operator)
  (set-macro-character #\= #'read-arithmetic-operator)
  (set-macro-character #\< #'read-arithmetic-dipthong)
  (set-macro-character #\> #'read-arithmetic-dipthong)
)

(defun cleanup-readtable ()
  ;; Standard way to restore readtable.
  (setf cl:*readtable* (copy-readtable nil)))


;;;
;;; Reading program forms
;;;
(defun read-thing (str start)
  "Read a logo object from somewhere in a string."
  (let ((*package* (find-package "LOGO")))
    (read-from-string str nil nil :start start)))

(defun make-logo-symbol (string)
  (let* ((package (find-package "LOGO")))
    (shadow (make-symbol string) package)
    (intern (format nil "~A" string) package)))


(defun prompted-read-line (level &optional (stream *standard-input*))
  "If interactive, prompt then read a line of characters. Otherwise just
read the line. Take into account

1. Continuation characters
2. Reading a quoted list, i.e. [foo bar baz]. The idea here is to
   avoid the need for continuation characters when reading long
   lists (like runlists commonly are).

Return the resulting line as a single line without comments or
continuation characters."

  ;; Set prompt to LEVEL+1 instances of the prompt character.
  (let ((*prompt* (format nil "~A" (make-string (1+ level) :initial-element *prompt-char*))))
    (prompt))

  (flet ((append-continued-line (line level)
	   (concatenate 'string
			(string-right-trim '(#\space #\tab #\~) line)
			" "
			(string-left-trim *whitespace-bag* (prompted-read-line level stream)))))
  
    (let* ((line (string-right-trim *whitespace-bag* (read-line stream))))

      (unless (string= line "")

	;; Handle continued lines.
	(when (char= (aref line (- (length line) 1)) #\~)
	  (setf line (append-continued-line line level)))

	;; Handle reading bracketed lists. These get turned into quoted
      ;; lists by the reader.
      (let* ((level (+ level (cl:count #\[ line)))
	     (level (- level (cl:count #\] line))))

	(when (> level 0)
	  ;; Still reading a list
	  (setf line (append-continued-line line level)))))

      line)))


(defun read-instructions (&optional (stream *standard-input*))
  "Read a (possibly continued, that is, multiple input lines
concatenated) line containing one or more logo instructions."
  (string-right-trim *whitespace-bag* (prompted-read-line 0 stream)))


(defun match-form (string line)
  (let ((sl (length string)))
    (and (>= (length line) sl)
	 (string-equal string line :end2 sl))))

;;;
;;; Right now only LOAD and TO seem to need special treatment.

(defun to-form (instruction-line)
  (match-form "TO " instruction-line))

(defun load-form (instruction-line)
  (match-form "LOAD " instruction-line))

(defun local-form (instruction-line)
  (match-form "LOCAL " instruction-line))

(defun localmake-form (instruction-line)
  (match-form "LOCALMAKE " instruction-line))

;;; This isn't finished yet.
#+cmu
(defun edit-form (instruction-line)
  (match-form "EDIT " instruction-line))

(defun output-form (instruction-line)
  (match-form "OUTPUT " instruction-line))

(defun stop-form (instruction-line)
  (match-form "STOP " instruction-line))


(defun logo-special-form-p (instruction-line)
  "See if the line is a special form."
  (or
   (load-form instruction-line)
   (local-form instruction-line)
   (localmake-form instruction-line)
   (to-form instruction-line)
   ;; #+cmu (edit-form instruction-line)
   (output-form instruction-line)
   (stop-form instruction-line)
   ))

(defun append-procedure-text (instruction-line)
  "Accumulate the procedure text to store in the procedure structure."
  (setf *procedure-text*
	(format nil "~A~%~A"
		*procedure-text*
		instruction-line)))

(defun maybe-find-procedure (atom)
  "Return the actual function name if there is one."
  (let ((proc (find-procedure-from-name atom))) ; This is still the namestring.
    (if proc 
	(logo-procedure-procedure proc)
	atom)))

(defun parse-thing (thing)
  "Take a thing (atom or list) and translate whatever is translatable as
a procedure."
  (cond ((quoted-p thing) thing)
	((atom thing) (maybe-find-procedure thing))
	(t (cons (parse-thing (car thing)) (parse-thing (cdr thing))))))



(defun parse-instructions (instruction-line)
  "Read a thing and translate the symbols (if any) in it. If it's a
quoted thing, leave it alone and just return it."
  (let ((output nil)
	(thing nil)
	(position 0))
    (loop
      ;; Loop through the "things" in the line.
     (multiple-value-setq (thing position)
       (read-thing instruction-line position))
      (if thing
	  (if (quoted-p thing)      ; Either "foo or [foo bar baz]
	      (push thing output) ; Just leave it alone
	      ;; We have an unquoted thing. Parse it.
	      (let ((newthing (parse-thing thing)))
		(push newthing output)))
	  (return (nreverse output))))))


(defun read-form ()
  "Parse a logo form (a line) and return a list containing lisp
expressions in the line(s). Converts from strings of logo syntax to
lists of lisp syntax using the *readtable* mechanism. Sets the package
to :logo so all the symbols will wind up in the :logo package. Calls
TRANSLATE-SPECIAL-FORM to intercept things that need to be handled
specially, such as LOAD and TO."
  (let ((*package* (find-package "LOGO"))
	(line ""))
    (loop
      ;; May have to read past comment lines in the procedure. We do
      ;; this here so we can preserve the comments in the procedure text.
      (let* ((instruction-line (read-instructions))
	     ;; If no comment, position will return nil so we get the
	     ;; whole line in the subseq call.
	     (comment-start (cl:position #\; instruction-line))) 
	(setf line (string-right-trim *whitespace-bag* (subseq instruction-line 0 comment-start)))
	(when (defining-procedure)
	  (append-procedure-text instruction-line))
	(unless (string= line "") ; Throw out blank lines or lines
				  ; zeroed out as comment lines.
	  (return))))
    (if (logo-special-form-p line)
	(translate-special-form line)
	(parse-instructions line))))

(defun test-reader ()
  (unwind-protect
      (progn
	(setup-logo-readtable)
	(read-form))
    (setf *readtable* (copy-readtable nil))))


(defun prompt ()
  "Print the logo prompt.  The prompt is stored in the *prompt* global
variable."
  (when *interactive*
    (format *query-io* "~A " *prompt*)
    (force-output *query-io*)))


(defun defining-procedure ()
  "We are defining a procedure when there is a current procedure."
  ;; *defining-procedure*
  *current-procedure*)
