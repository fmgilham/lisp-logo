;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOGO -*-
;;;
;;; Time-stamp: <2022-12-31 05:58:21 fred>
;;; Logo-to-lisp translator: turtle.lisp
;;; Turtle graphics.  This code had its origins in a file called
;;; turtle.l by Gunther Schadow that seemed to have come with akcl.
;;; Original: FMG 8-AUG-1999
;;;  

(in-package "LOGO")

(declaim #.*compile-options*)
	  
;;; Here's turtle things.
;;;
;;; The turtle things reflect the `turtle-world' as opposed to the X
;;; Window world. So the heading represents an angle in degrees from
;;; vertical, and x and y are relative to the center of the window,
;;; with up and right being positive, and down and left being
;;; negative.
;;;
;;; We have functions to translate between `turtle-world' coordinates
;;; and X Window world coordinates.
;;;

(defstruct turtle
  (x              0.0   :type cl:number)
  (y              0.0   :type cl:number)
  (heading        0.0   :type cl:number)
  (visible-p        t   :type (member t nil))
  ;; Turtles have their own turtle mode. I guess this makes sense. But
  ;; that means when we have multiple turtles the primitive that sets
  ;; the turtle mode will have to take a turtle parameter.
  (turtle-mode  :wrap   :type (member :bounce :fence :window :wrap))
  (pen-position :down   :type (member :down :up))
  (pen-mode     :paint  :type (member :paint :erase :reverse))
  (color            0)
  (thread         nil) ; Thread associated with this turtle (by "task" procedure).
#-(and)  (runlist        nil) ; Code this turtle is running as a process.
  (name           'nil  :type cl:symbol)
  (gc             nil)
;;  (line-width 1 :type fixnum)
)


;;; X-related utilities
;;;

(defun map-state (w)
  (xlib:with-state (w)
    (xlib:window-map-state w)))

(defun wait-for-mapping (display win)
  (do ((mapped (map-state win) (map-state win)))
      ((eq mapped :viewable))
    (sleep 0)
    (xlib:display-finish-output display)))

(defun wait-for-unmapping (display win)
  (do ((mapped (map-state win) (map-state win)))
      ((eq mapped :unmapped))
    (sleep 0)
    (xlib:display-finish-output display)))

(defun cleanup-graphics ()
  (when *display*
    (xlib:close-display *display*)
    (setf *display* nil
	  *screen* nil
	  *root* nil
	  *colormap* nil
	  *black-pixel* nil
	  *white-pixel* nil
	  *turtle-window* nil)))

(defun setup-colors ()
  (setf *color-array* 
	(cl:make-array (length *color-names*) :element-type 'xlib:card32 :initial-element 0))
  (dotimes (i (length *color-names*))
    (setf (aref *color-array* i)
	  (xlib:alloc-color (xlib:screen-default-colormap *screen*) (aref *color-names* i)))))

(defun turtle-graphics ()
  ;;  (unless *display*
  (setf *display* (xlib:open-default-display))
  (setf *screen* (xlib:display-default-screen *display*))
  (setf *root* (xlib:screen-root *screen*))
  (setf *colormap* (xlib:screen-default-colormap *screen*))
  (setf *black-pixel* (xlib:screen-black-pixel *screen*))
  (setf *white-pixel* (xlib:screen-white-pixel *screen*))
  (setf *turtle-window* (xlib:create-window :parent *root*
					    :x 100 :y 100
					    :event-mask '(:visibility-change :structure-notify :key-press)
					    :width *window-width*
					    :height *window-height*
					    :backing-store :when-mapped
					    :background *black-pixel*
					    :border *white-pixel*
					    :border-width 2))
  (xlib:set-wm-properties *turtle-window*
			  :name "Turtle Graphics"
			  :icon-name "Turtle Graphics"
			  :resource-name "Turtle Graphics"
			  :x 100 :y 100
			  :width *window-width* :height *window-height*
			  :user-specified-position-p t
			  :user-specified-size-p t
			  :min-width *window-width* :min-height *window-height*
			  :width-inc nil :height-inc nil)

  (let ((font (xlib:open-font *display* *turtle-normal-font*)))

    ;; when penmode is paint
    (setf *turtle-paint-gc*
	  (xlib:create-gcontext :drawable *turtle-window*
				:background *black-pixel*
				:foreground *white-pixel*
				:cap-style :not-last
				:font font
 				:line-width 0 :line-style :solid
				:function boole-1))

    ;; When penmode is erase
    (setf *turtle-erase-gc*		; Draw background color
 	  (xlib:create-gcontext :drawable *turtle-window*
 				:background *white-pixel*
 				:foreground *black-pixel*
				:cap-style :butt
				:font font
 				:line-width 0 :line-style :solid
 				:function boole-1))

    (setf *turtle-reverse-gc*		; Draw background color
 	  (xlib:create-gcontext :drawable *turtle-window*
 				:background *black-pixel*
 				:foreground *white-pixel*
				:cap-style :not-last
				:font font
 				:line-width 0 :line-style :solid
 				:function boole-xor))

;;;; (setf *turtle-up-gc*      ; GC that does nothing.
;;;; 	(xlib:create-gcontext :drawable *turtle-window*
;;;; 			      :background *white-pixel*
;;;; 			      :foreground *black-pixel*
;;;; 			      :line-width 0 :line-style :solid
;;;; 			      :function boole-2))


    ;; The GC used for drawing the turtle
    (setf *turtle-draw-gc*
	  (xlib:create-gcontext :drawable *turtle-window*
				:background *black-pixel*
				:foreground *white-pixel*
				:cap-style :not-last
				:line-width 0 :line-style :solid
				:function boole-xor)))
  
  (setf (symbol-value 'studs)
	(make-turtle :name 'studs :gc *turtle-paint-gc*)) ; From THINKING ABOUT [TLC] LOGO
  (setf *current-turtle* (symbol-value 'studs))
  ;; Turtle used to draw the turtle itself.
  (setf *turtle-draw-turtle* (make-turtle :turtle-mode :window :gc *turtle-draw-gc*))
  (setf *turtles* nil)
  (push *current-turtle* *turtles*)

  (xlib:map-window *turtle-window*)
  (wait-for-mapping *display* *turtle-window*)
  (setup-colors)
  )


(declaim (inline ensure-graphics))
(defun ensure-graphics ()
  (unless *display* (turtle-graphics)))

(defun get-key ()
  "Get keystrokes in the turtle window and translate to characters."
  (ensure-graphics) ; Need a turtle window for this to work!
  (xlib:event-case (*display* :timeout .1 :force-output-p t :discard-p t)
    (:key-press 
     (display code state)
     (let* ((char (xlib:keycode->character display code state))
	    (sym (make-logo-symbol (string-upcase (format nil "~A" char)))))
       sym))))


;;; Housekeeping.
;;;

(defmacro with-redrawn-turtle (&body forms)
  "Macro to wrap functions that change the turtle so it gets redrawn
correctly. XXX this doesn't really work right with multiple turtles."
  `(unwind-protect
	(progn
	  (ensure-graphics)
	  (dolist (turtle *turtles*)
	    (draw-turtle turtle))	; This will erase the turtle because it uses the boole-xor GC operation.
	  ,@forms
	  (values))
     (dolist (turtle *turtles*)
       (draw-turtle turtle))))     ; Now redraw the turtle


(defun update-window-system-state ()
  "Stuff in here should be done every 'cycle' of the toplevel. For
example, the current function updates the system if the user resizes
the window."
  (let ((redraw-flag nil))
    (when *display*
      (xlib:with-state (*turtle-window*)
	(when (not (= (xlib:drawable-height *turtle-window*) *window-height*))
	  (setf *window-height* (xlib:drawable-height *turtle-window*))
	  (setf redraw-flag t))
	(when (not (= (xlib:drawable-width *turtle-window*) *window-width*))
	  (setf *window-width* (xlib:drawable-width *turtle-window*))
	  (setf redraw-flag t)))
      (when redraw-flag (clear-screen)))))

;;; The code is mostly written so that all the implementation
;;; functions that take a turtle as an argument are named turtle-<foo>
;;; whereas the ones that directly interface with the user are named
;;; <foo> and pass the *current-turtle* to the turtle-<foo> function.
;;;
;;; If the turtle draws over itself, it will be erased. This is
;;; because of using the boole-xor drawing operation. Note: we use the
;;; same "draw turtle" to draw turtles (when there are multiple
;;; turtles). It will draw the *current-turtle*. XXX This don't work
;;; so good....

(defun draw-turtle (turtle)
  (unless *deferred-redraw*
    (when (turtle-visible-p turtle)
      (setf (turtle-x *turtle-draw-turtle*) (turtle-x turtle)
	    (turtle-y *turtle-draw-turtle*) (turtle-y turtle)
	    (turtle-turtle-mode *turtle-draw-turtle*) :window ; Don't compensate for turtle mode
	    (turtle-heading *turtle-draw-turtle*) (+ 90 (turtle-heading turtle)))
      (do-draw-turtle))))

(defun do-draw-turtle ()
  (let ((turtle *turtle-draw-turtle*))
    (turtle-set-pen-position turtle :up)
    (turtle-forward turtle -10)
    (turtle-set-pen-position turtle :down)
    (turtle-forward turtle 20)

    (setf (turtle-heading turtle) (+ 240 (turtle-heading turtle)))
    (turtle-forward turtle 20)
    (setf (turtle-heading turtle) (+ 240 (turtle-heading turtle)))
    (turtle-forward turtle 20)))


;;; Coordinate transformations
;;;

(declaim (inline turtle-x-to-window-x window-x-to-turtle-x turtle-y-to-window-y window-y-to-turtle-y))

(defun turtle-x-to-window-x (tx)
  "Transform an x coordinate from turtle coordinate to X window
coordinate."
  (let ((scrunched (* tx *scrunch-x*))
	(offset (/ *window-width* 2)))
    (+ scrunched offset)))

(defun window-x-to-turtle-x (wx)
  "Transform an x coordinate from X window coordinate to a turtle
coordinate."
  (let* ((offset (/ *window-width* 2))
	 (xsc (- wx offset)))
    (/ xsc *scrunch-x*)))

(defun turtle-y-to-window-y (ty)
  "Transform a y coordinate from turtle coordinate to X window
coordinate. Note that the X window y coordinate increases downward,
while the turtle y coordinate decreases downward."
  (let* ((scrunched (* (- ty) *scrunch-y*))
	 (offset (/ *window-height* 2)))
    (+ scrunched offset)))

(defun window-y-to-turtle-y (wy)
  "Transform a y coordinate from X window coordinate to turtle
coordinate. Note that the X window y coordinate increases downward,
while the turtle y coordinate decreases downward."
  (let* ((offset (/ *window-height* 2))
	 (ysc (- offset wy)))
   (/ ysc *scrunch-y*)))

;;;
;;; Degrees to radians and vice versa
;;;
(defun deg (rad)
  "Radians to degrees"
  (* rad (/ 180.0 cl:pi)))

(defun rad (deg)
  "Degrees to radians"
  (* deg (/ cl:pi 180.0)))


(defun window-heading (h)
  "From turtle heading to X window heading"
  (+ (- h) 90.0))

;;;
(defun window-x (turtle)
  "From turtle X coordinate to X window coordinate."
  (turtle-x-to-window-x (turtle-x turtle)))

(defun window-y (turtle)
  "From turtle Y coordinate to X window coordinate."
  (turtle-y-to-window-y (turtle-y turtle)))

(defun heading ()
  (ensure-graphics)
  (turtle-heading *current-turtle*))


;;;
;;; Turtle graphic functions.
;;;

(defun turtle-draw-point (turtle)
  (xlib:draw-point *turtle-window* (turtle-gc turtle)
		   (cl:round (min (window-x turtle) 16383))
		   (cl:round (min (window-y turtle) 16393)))
  (xlib:display-force-output *display*))

(defun draw-point ()
  (with-redrawn-turtle
    (turtle-draw-point *current-turtle*)))


(defun turtle-arc (turtle angle size)
  (when (eq (turtle-pen-position turtle) :down)
    (let* ((heading (rad (window-heading (turtle-heading turtle))))
	   (arc-origin-x (cl:round (- (window-x turtle) (/ size 2))))
	   (arc-origin-y (cl:round (- (window-y turtle) (/ size 2)))))
      (xlib:draw-arc *turtle-window* (turtle-gc turtle)
		     arc-origin-x arc-origin-y
		     size size
		     heading angle)
      (xlib:display-force-output *display*))))

(defun arc (angle size)
  (with-redrawn-turtle
    (let ((ang (if (> angle 360) (+ 360 (mod angle 360)) angle)))
      (turtle-arc *current-turtle* (- (rad ang)) size))))

(defun circle (size)
  (with-redrawn-turtle
    (turtle-arc *current-turtle* (* cl:pi 2) size)))


;;; Line draw. This takes into account the turtle modes.
;;;

(defun turtle-do-window-line-to (turtle x1 y1 x2 y2)
  "Draw a line from x1,y1 to x2,y2 in window coordinates."
  (when (eq (turtle-pen-position turtle) :down)
    (xlib:draw-line *turtle-window*
		    (turtle-gc turtle)
		    (cl:round x1)
		    (cl:round y1)
		    (cl:round x2)
		    (cl:round y2))
    (xlib:display-force-output *display*))
  ;; Update turtle to reflect "reality".
  (setf (turtle-x turtle) (window-x-to-turtle-x x2)
	(turtle-y turtle) (window-y-to-turtle-y y2)))


;; Doesn't look like this is useful---need to take scrunch into
;; account.
(defun distance (x1 x2 y1 y2)
  (sqrt (+ (power (- x2 x1) 2) (power (- y2 y1) 2))))

;;;
;;; dist = sqrt ((x1/sx - x2/sx)^2 + (y1/sy - y2/sy)^2)
;;;      = sqrt (((x1 - x2)/sx)^2 + ((y1 - y2)/sy)^2)
;;;      = sqrt ((xdiff/sx)^2 + (ydiff/sy)^2)
;;;      = sqrt ((sy * xdiff)^2 + (sx * ydiff)^2) / (sx*sy)^2
(defun scaled-distance (x1 x2 y1 y2)
  "Distance in window units, taking scrunches into account. Ugh. X and Y
scrunch may be different."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((sx *scrunch-x*)
	(sy *scrunch-y*)
	(xdiff (- x1 x2))
	(ydiff (- y1 y2)))
    (sqrt (/ (+ (* sy sy xdiff xdiff)
		(* sx sx ydiff ydiff))
	     (* sx sx sy sy)))))

;;;
;;; We have similar triangles. P0 is where the turtle is. P2 is the
;;; "target" point. P1 is where we hit the Y border (or X border).
;;;
;;; dist(p0, p2) = sqrt((x2 - x0)^2 + (y2 - y0)^2)
;;;
;;; dist(p0, p2)    dist(x0, x2)   dist(y0, y2)
;;; ------------ =  ------------ = ------------
;;; dist(p0, p1)    dist(x0, x1)   dist(y0, y1)
;;;
;;;
;;; x1 - x0     dist(p0, p1)    y1 - y0
;;; --------- = ------------ = -------
;;; x2 - x0     dist(p0, p2)    y2 - y0
;;;
;;;           (y1 - y0) (x2 - x0)
;;; x1 - x0 = -------------------
;;;                 y2 - y0
;;;
;;;
;;;      (x2 - x0) (y1 - y0)
;;; x1 = ------------------- + x0
;;;            y2 - y0
;;;
;;; y1 is the corresponding formula with y substituted for x and vice
;;; versa.
;;;
(defun do-funky-turtle-modes (turtle x y)
  "Implements the turtle modes: :wrap, :bounce and :fence. :window mode
is implemented in TURTLE-LINE-TO.

X and Y are the window coordinates of the target.

We know one of X and Y is out of bounds (perhaps both). Find the
length of the part of the line that's in bounds and draw it. Modify
the turtle appropriately, then call TURTLE_FORWARD to draw the rest of
the line. If that part of the line goes out of bounds, recursion will
take care of it.

If mode is :fence, just draw the first segment and signal the error."
  (declare (optimize (speed 3) (safety 1) (debug 1) (space 0)))
  (let* ((mode (turtle-turtle-mode turtle))
	 (x-intercept (if (plusp x) *window-width* 0))
	 (y-intercept (if (plusp y) *window-height* 0))
	 (flag-x (not (<= 0 x *window-width*)))
	 (flag-y (not (<= 0 y *window-height*)))
	 (x0 (window-x turtle))  ; The current location of the
	 (y0 (window-y turtle))  ; turtle in window coordinates.
	 ;; For similar triangle calculation; keep points organized.
	 ;; X0 and y0 are the current coordinates of the turtle. X1
	 ;; and y1 are where the turtle would hit some window border,
	 ;; and x2 and y2 are the target coordinates.
	 (x2 x)
	 (y2 y)
	 ;; We calculate x1, which is the X coordinate when the line
	 ;; hits the Y border. If y0 and y2 are equal, kludge around
	 ;; the division by 0.
	 (x1 (+ x0 (/ (* (- x2 x0) (- y-intercept y0)) (if (= y0 y2) .01 (- y2 y0)))))
	 ;; We calculate y1, which is the Y coordinate when the line
	 ;; hits the X border. Kludge to prevent division by 0.
	 (y1 (+ y0 (/ (* (- y2 y0) (- x-intercept x0)) (if (= x0 x2) .01 (- x2 x0))))))

    (when (and flag-x flag-y)
      ;; The line is long enough to hit both borders. We need to
      ;; figure out which border is closer, then clear the flag of the
      ;; one that's farther away. That one will be taken care of when
      ;; we make the call to TURTLE-FORWARD below.
      (let ((distance-x-border (scaled-distance x0 x-intercept y0 y1))
	    (distance-y-border (scaled-distance x0 x1 y0 y-intercept)))
	(if (>= distance-x-border distance-y-border)
	    (setf flag-x nil)
	    (setf flag-y nil))))

    (dformat "~&Turtle mode is ~A~%" mode)
    (dformat "~&Flag-X is ~A; Flag-Y is ~A~%" flag-x flag-y)

    (unless (or flag-x flag-y)
      (error "Do-funky-turtle-modes: Both flags clear. How did we get here?"))

    (let* ((new-x (if flag-x x-intercept x1))
	   (new-y (if flag-y y-intercept y1))
	   (d-tot (scaled-distance x0 x2 y0 y2))
	   (d-part (scaled-distance x0 new-x y0 new-y))
	   (remaining-r (- d-tot d-part)))

      ;; Draw line segment from current turtle position to border.
      (dformat "~&Drawing first segment from ~A,~A to ~A,~A~%" x0 y0 new-x new-y)
      ;; This will update turtle-x and turtle-y appropriately.
      (turtle-do-window-line-to turtle x0 y0 new-x new-y)

      ;; Now adjust the turtle to account for the turtle turtle mode.
      (case mode

	(:wrap
	 ;; We want to draw the second line. But if that line hits a
	 ;; window border, we set the turtle position to the other
	 ;; border.
       
	 (if flag-x 
	     ;; Turtle is on an X boundary now. Switch it to the
	     ;; other X boundary.
	     (setf (turtle-x turtle) (minus (turtle-x turtle)))
	     ;; Turtle is on a Y boundary, switch it to the other Y
	     ;; boundary.
	     (setf (turtle-y turtle) (minus (turtle-y turtle)))))

	(:bounce
	 ;; Invert the turtle heading. 

	 (if flag-x
	     ;; X boundary. HEADING becomes 0 - HEADING or simply - HEADING.
	     (setf (turtle-heading turtle) (- (turtle-heading turtle)))
	     ;; Y boundary. HEADING becomes 180 - HEADING.
	     (setf (turtle-heading turtle) (- 180 (turtle-heading turtle)))))

	(:fence
	 ;; Report an error for the appropriate axis.
	 
	 (if flag-x
		    (error 'logo-out-of-bounds :axis 'x :coordinate (coerce (turtle-x turtle) 'float))
		    (error 'logo-out-of-bounds :axis 'y :coordinate (coerce (turtle-y turtle) 'float)))))

      (turtle-forward turtle remaining-r))))


(defun out-of-bounds-check (x y)
  "Return t if one of the window coordinates is out of bounds"
  (or (not (<= 0 x *window-width*))
      (not (<= 0 y *window-height*))))


(defun turtle-line-to (turtle x y)
  "Draw a line from where the turtle is to some point given by X and
Y (in turtle coordinates)."
  (let ((old-x (window-x turtle))
	(old-y (window-y turtle))
	(new-x (turtle-x-to-window-x x))
	(new-y (turtle-y-to-window-y y)))

    ;; We are now in window coordinates.
    ;; Don't check :window mode for being out of bounds.
    (if (and (not (eq (turtle-turtle-mode turtle) :window)) (out-of-bounds-check new-x new-y))
	;; Turtle out of bounds.
	(do-funky-turtle-modes turtle new-x new-y)
	;; Turtle in bounds. TURTLE-DO-WINDOW-LINE-TO updates the turtle's
  	;; coordinates
	(turtle-do-window-line-to turtle old-x old-y new-x new-y))))

(defun line-to (x y)
  (with-redrawn-turtle
    (turtle-line-to *current-turtle* x y)))

(defun turtle-draw-label (turtle text)
  (xlib:draw-glyphs *turtle-window* (turtle-gc turtle)
		    (cl:round (window-x turtle))
		    (cl:round (window-y turtle))
		    text)
  (xlib:display-force-output *display*))

(defun label (input &rest inputs)
  (ensure-graphics)
  (let ((stream (make-string-output-stream)))
    (if inputs
	(progn
	  (print-aux (unquote input) t stream)
	  (dolist (more (butlast inputs))
	    (princ " " stream)
	    (print-aux more t stream))
	  (princ " " stream)
	  (print-aux (last inputs) t stream))
	(print-aux (unquote input) t stream))
    (turtle-draw-label *current-turtle* (get-output-stream-string stream))))



;;; Turtle position
(defun turtle-home-position (turtle)
  (setf (turtle-heading turtle) 0.0)    ; is at 12 o' clock
  (setf (turtle-x turtle) 0.0)
  (setf (turtle-y turtle) 0.0))

(defun home-position ()
  (with-redrawn-turtle
    (turtle-home-position *current-turtle*)))

(defun turtle-home (turtle)
  (turtle-line-to turtle 0.0 0.0)
  (turtle-home-position turtle))

(defun home ()
  (with-redrawn-turtle
    (turtle-home *current-turtle*)))

(defun xcor ()
  (ensure-graphics)
  (turtle-x *current-turtle*))

(defun ycor ()
  (ensure-graphics)
  (turtle-y *current-turtle*))

(defun position ()
  (let ((turtle *current-turtle*))
    (cl:list (turtle-x turtle) (turtle-y turtle))))

(defun turtle-set-pos (turtle x y)
  (turtle-line-to turtle x y))

(defun set-xy (x y)
  (with-redrawn-turtle
    (turtle-set-pos *current-turtle* x y)))

(defun set-x (x)
  (with-redrawn-turtle
    (turtle-set-pos *current-turtle* x (turtle-y *current-turtle*))))

(defun set-y (y)
  (with-redrawn-turtle
    (turtle-set-pos *current-turtle* (turtle-x *current-turtle*) y)))

(defun set-position (l)
  (with-redrawn-turtle
    (let ((ll (unquote l)))
      (turtle-set-pos *current-turtle* (car ll) (cadr ll)))))

(defun turtle-forward (turtle r)
  (let* ((heading (rad (window-heading (turtle-heading turtle))))
	 (old-x (turtle-x turtle))
	 (old-y (turtle-y turtle))
	 (new-x (+ old-x (* r (cl:cos heading))))
	 (new-y (+ old-y (* r (cl:sin heading)))))
    (turtle-line-to turtle new-x new-y)))

(defun forward (r)
  (with-redrawn-turtle
    (turtle-forward *current-turtle* r)))

(defun back (r)
  (with-redrawn-turtle
    (turtle-forward *current-turtle* (- r))))


;;; Heading
;;;

(defun turtle-set-heading (turtle deg)
  (setf (turtle-heading turtle) deg))

(defun set-heading (deg)
  "Absolute heading."
  (with-redrawn-turtle
    (turtle-set-heading *current-turtle* deg)))

(defun turtle-rotate (turtle angle)
  "Relative heading. Rotate turtle; positive phi means rotate to the
right."
  (let ((heading (turtle-heading turtle)))
    (setf (turtle-heading turtle) (mod (+ heading angle) 360.0))))

(defun right-turn (angle)
  (with-redrawn-turtle
    (turtle-rotate *current-turtle* angle)))

(defun left-turn (angle)
  (with-redrawn-turtle
    (turtle-rotate *current-turtle* (- angle))))


(defun turtle-towards (turtle coord)
  (let ((x (turtle-x turtle))
	(y (turtle-y turtle))
	(xcoord (cl:first coord))
	(ycoord (cl:second coord)))
    (arctan (- ycoord y) (- xcoord x))))

(defun towards (coord)
  (turtle-towards *current-turtle* (unquote coord)))



;;; Turtle attributes
;;;

(defun turtle-set-pen-color (turtle color-thing)
  (let ((color (unquote color-thing)))
    (unless (or (and (numberp color) (<= 0 color (1- (length *color-array*))))
		(and (cl:listp color) (= (length color) 3)))
      (let ((emsg (format nil "list of three non-negative numbers 1.0 or less~%
or a single non-negative integer less than ~A" 
			  (length *color-array*))))
	(error 'logo-type-error :procedure-name 'setpencolor :datum color :expected-type emsg)))
    (let ((index
	    (if (numberp color) 
		(aref *color-array* color)
		;; Apparently you have to do this complexity to call xlib:alloc-color. Hangs otherwise.
		(let (disp)
		  (unwind-protect
		       (destructuring-bind (r g b) color
			 (setf disp (xlib:open-default-display))
			 (let* ((scr (xlib:display-default-screen disp))
				(color (xlib:alloc-color 
					(xlib:screen-default-colormap scr)
					(xlib:make-color :red r :green g :blue b))))
			   color))
		    (xlib:close-display disp)))))
	  (gc (turtle-gc turtle)))
      (setf (turtle-color turtle) color)
      (setf (xlib:gcontext-foreground gc) index))))

;; Visibility

(defun hide-turtle ()
  (with-redrawn-turtle ; Won't redraw turtle because turtle-visible-p becomes nil
    (setf (turtle-visible-p *current-turtle*) nil)))

(defun show-turtle ()
  (with-redrawn-turtle
    (setf (turtle-visible-p *current-turtle*) t)))

;; Pen color

(defun set-pen-color (color)
  (with-redrawn-turtle
    (turtle-set-pen-color *current-turtle* color)))

(defun pen-color ()
  (ensure-graphics)
  (turtle-color *current-turtle*))

;; Pen size

(defun pen-size ()
  (ensure-graphics)
  (let* ((gc (turtle-gc *current-turtle*))
	 (line-width (xlib:gcontext-line-width gc)))
    (cl:list line-width line-width)))

;; Pen up or down (not where turtle is)
(defun turtle-set-pen-position (turtle state)
  (setf (turtle-pen-position turtle) state))

#-(and) ; not yet
(defun pen-down ()
  (ensure-graphics)
  (case (turtle-pen-mode *current-turtle*)
    (paint (setf (turtle-gc *current-turtle*) *turtle-paint-gc*))
    (erase (setf (turtle-gc *current-turtle*) *turtle-erase-gc*))
    (reverse (setf (turtle-gc *current-turtle*) *turtle-reverse-gc*)))
  (draw-turtle)
  )

#+(and) ; old way
(defun pen-down ()
  (ensure-graphics)
  ; Don't erase or redraw the turtle.
  (turtle-set-pen-position *current-turtle* :down)
  (values))

#-(and) ; not yet
(defun pen-up ()
  (ensure-graphics)
  (setf (turtle-gc *current-turtle*) *turtle-up-gc*)
  (draw-turtle)
  )

#+(and) ; old-way
(defun pen-up ()
  (ensure-graphics)
  ; Don't erase or redraw the turtle.
  (turtle-set-pen-position *current-turtle* :up)
  (values))


(defun pen-position ()
  (ensure-graphics)
  (turtle-pen-position *current-turtle*))

(defun pen-erase ()
  (with-redrawn-turtle
    (setf (turtle-gc *current-turtle*) *turtle-erase-gc*)
    (setf (turtle-pen-mode *current-turtle*) :erase)))

(defun pen-paint ()
  (with-redrawn-turtle
    (setf (turtle-gc *current-turtle*) *turtle-paint-gc*)
    (setf (turtle-pen-mode *current-turtle*) :paint)))

(defun pen-reverse ()
  (with-redrawn-turtle
    (setf (turtle-gc *current-turtle*) *turtle-reverse-gc*)
    (setf (turtle-pen-mode *current-turtle*) :reverse)))

(defun pen-mode ()
  (turtle-pen-mode *current-turtle*))

;;; turtle mode.
;;;
;;; :bounce --- turtle bounces off the edges of the window in
;;;             accordance with some conception of the laws of
;;;             physics.
;;;
;;; :fence  --- turtle gives an error when it goes past the borders of
;;;             the window.
;;;
;;; :window --- treats window as viewport on an infinite plane. Turtle
;;;             just "keeps going" when it goes past the borders.
;;;
;;; :wrap   --- turtle shows up on other side when it passes the
;;;             borders.
;;;

(defun turtle-set-turtle-mode (turtle mode)
  (setf (turtle-turtle-mode turtle) mode))

(defun bounce ()
  (turtle-set-turtle-mode *current-turtle* :bounce)
  (values))

(defun fence ()
  (turtle-set-turtle-mode *current-turtle* :fence)
  (values))

(defun window ()
  (turtle-set-turtle-mode *current-turtle* :window)
  (values))

(defun wrap ()
  (turtle-set-turtle-mode *current-turtle* :wrap)
  (values))

(defun turtle-mode (turtle)
  (turtle-turtle-mode turtle))

(defun mode ()
  (turtle-mode *current-turtle*))

;;; Turtle state

(defun shownp ()
  (if (turtle-visible-p *current-turtle*) 'true 'false))

;; Pen size

(defun turtle-set-pen-size (turtle spec)
  (let ((gc (turtle-gc turtle)))
    (setf (xlib:gcontext-line-width gc) 
	  (if (consp spec)
	      (second spec)
	      spec))))

(defun set-pen-size (spec)
  (with-redrawn-turtle
    (turtle-set-pen-size *current-turtle* spec)))

;; Scrunch. Compensate for aspect ratio or scale drawings without
;; re-doing all the random numbers in the procedure

;; XXX Would be nice to save and resize what is currently on the
;; window.
(defun set-scrunch (scrunch-x scrunch-y)
  (when (or (not (plusp scrunch-x)) (not (plusp scrunch-y)))
    (error 'logo-type-error :procedure-name 'scrunch :datum (cl:list scrunch-x scrunch-y) :expected-type "two positive numbers"))
  (with-redrawn-turtle
    (xlib:clear-area *turtle-window*)
    (setf *scrunch-x* scrunch-x
	  *scrunch-y* scrunch-y)))

(defun scrunch ()
  (cl:list *scrunch-x* *scrunch-y*))

;; Fonts.

(defun turtle-set-font (turtle font)
  (setf (xlib:gcontext-font (turtle-gc turtle))
	(case font
	  (tiny *turtle-tiny-font*)
	  (small *turtle-small-font*)
	  (normal *turtle-normal-font*)
	  (large *turtle-large-font*)
	  (huge *turtle-huge-font*))))
		       

(defun set-font (font)
  (unless (member font '(tiny small normal large huge))
    (error 'logo-type-error 
	   :procedure-name 'setfont 
	   :datum font 
	   :expected-type "one of tiny, small, normal, large or huge"))
  (turtle-set-font *current-turtle* font)
  (values))


;;; Screen
;;;

(defun clear-screen ()
  (with-redrawn-turtle
    (xlib:clear-area *turtle-window*)
    (dolist (turt *turtles*)
      (turtle-home-position turt)
      (setf (turtle-pen-position turt) :down)
      ;;      (setf (turtle-color turt) 7)
      (turtle-set-pen-color turt 7) ; Reset turtle pen to white.
      (setf (turtle-visible-p turt) t))
    (xlib:display-force-output *display*)))

;;; Same as clear-screen but don't modify turtle state
(defun clean ()
  (with-redrawn-turtle
    (xlib:clear-area *turtle-window*)
    (xlib:display-force-output *display*)))



;;; Multiple turtles
;;;

(defun hatch (word)
  (proclaim '(cl:special word))
  (let ((turtle (make-turtle :name word :gc *turtle-paint-gc*)))
    (setf (symbol-value word) turtle)
    (push turtle *turtles*)
    (draw-turtle turtle))
  (values))


(defun breed (prefix count)
  (dotimes (i count)
    (let ((turtle-name (word prefix (1+ i))))
      (hatch turtle-name)))
  (values))

(defun tell (name)
  "Get the attention of turtle named NAME."
  (if (boundp name)
      (let ((turtle (symbol-value name)))
	(if (turtle-p turtle)
	    (with-redrawn-turtle
	      (setf *current-turtle* turtle))
	    (error 'logo-type-error :name 'tell :datum name :expected-type "name of a turtle")))
      (error 'logo-unbound-variable :name name :procedure-name 'tell)))


(defun tellall (runlist)
  "Give all turtles a set of instructions. RUN the runlist so it
rebuilds the runlist for each turtle."
  (let ((*deferred-redraw* t))
    (dolist (turtle *turtles*)
      (let ((*current-turtle* turtle))
	(run runlist))))
  (dolist (turtle *turtles*)
    (draw-turtle turtle)))

(defun tellbreed (breed runlist)
  "Give all turtles of a particular 'breed' a set of instructions. RUN
the runlist so it rebuilds the runlist for each turtle."
  (let ((*deferred-redraw* t))
    (dolist (turtle *turtles*)
      (when (zerop (search (symbol-name breed) (symbol-name (turtle-name turtle))))
	(let ((*current-turtle* turtle))
	  (run runlist)))))
  (dolist (turtle *turtles*)
    (draw-turtle turtle)))


(defun clear-turtles ()
  "Delete all the turtles except STUDS."
  (let ((turtles *turtles*))
    (setf *turtles* nil)
    (dolist (turtle turtles)
      (let ((name (turtle-name turtle)))
	(if (eq name 'studs)
	    ;; Keep STUDS around
	    (progn
	      (push turtle *turtles*)
	      (setf *current-turtle* turtle))
	    (progn
	      (format t "~&Clearing ~A~%" name)
	      (makunbound name)))))))


#+sbcl
(defun compile-task (runlist)
  "Compile the runlist as a function to be run as a thread."
  (let ((rl (build-runlist runlist)))
    (sb-ext:without-package-locks 
      (compile nil
	       `(lambda (turtle)
		  (declare (optimize (compilation-speed 3) (space 0) (safety 0) (sb-ext:inhibit-warnings 3)))
		  (let ((*current-turtle* turtle))
		    ,rl))))))

#+sbcl
(defmacro task (name runlist)
  "Run a runlist in a separate thread with a particular turtle as
thread-local '*current-turtle*'."
  `(let ((turtle (symbol-value ,name)))
     (unless (turtle-p turtle)
       (error 'logo-type-error :name 'tell :datum ,name :expected-type "name of a turtle"))
     (unless (consp ,runlist)
       (error 'logo-type-error :procedure-name "run"
			       :datum ,runlist
			       :expected-type "list of instructions"))
     (setf (turtle-thread turtle) 
	   (sb-thread:make-thread (compile-task ,runlist) 
				  :name (format nil "~A" ,name)
				  :arguments turtle))
     (values)))

#+cmu
(defvar *auto-yield* nil)
#+cmu
(defmacro task (name runlist)
  "Run a runlist in a separate thread with a particular turtle as
thread-local '*current-turtle*'."
  `(let ((turtle (symbol-value ,name)))
     (unless (turtle-p turtle)
       (error 'logo-type-error :name 'tell :datum ,name :expected-type "name of a turtle"))
     (unless (consp ,runlist)
       (error 'logo-type-error :procedure-name "run"
			       :datum ,runlist
			       :expected-type "list of instructions"))
     (unless *auto-yield*
       (setf lisp::*max-event-to-usec* 50000)
       (setf lisp::*max-event-to-sec* 0)
       (setf *auto-yield* t))
     (setf (turtle-thread turtle) 
	   (mp:make-process (compile-expand ,runlist) :name (format nil "~A" ,name)))
     (values)))

#+(and cmu nil)
(defun task (name runlist)
  (error "Sorry! This version can only do one thing at a time."))

#+ccl
(defmacro task (name runlist)
  "Run a runlist in a separate thread with a particular turtle as
thread-local '*current-turtle*'."
  `(let ((turtle (symbol-value ,name)))
     (unless (turtle-p turtle)
       (error 'logo-type-error :name 'tell :datum ,name :expected-type "name of a turtle"))
     (unless (consp ,runlist)
       (error 'logo-type-error :procedure-name "run"
			       :datum ,runlist
			       :expected-type "list of instructions"))
     (setf (turtle-thread turtle) 
	   (ccl:process-run-function (format nil "~A" ,name) (compile-expand ,runlist)))
     (values)))

#+sbcl
(defun stoptask (name)
  "Stop a turtle that is running a task (i.e. do a TERMINATE-THREAD on
the thread executing the turtle's task.)"
  (let* ((turtle (symbol-value name))
	 (thread (turtle-thread turtle)))
    (sb-thread:terminate-thread thread)))

#+cmu
(defun stoptask (name)
  "Stop a turtle that is running a task (i.e. do a PROCESS-KILL on
the process executing the turtle's task.)"
  (let* ((turtle (symbol-value name))
	 (thread (turtle-thread turtle)))
    (mp:destroy-process thread)))

#+ccl
(defun stoptask (name)
  "Stop a turtle that is running a task (i.e. do a process-kill on the
process executing the turtle's task.)"
  (let* ((turtle (symbol-value name))
	 (thread (turtle-thread turtle)))
    (ccl:process-kill thread)))

#+(and cmu nil)
(defun stoptask (name)
  (error "Sorry! This version can only do one thing at a time. No tasks to stop."))


(defun ask (turtle-name accessor)
  "The accessor must be one of the automatically generated structure
accessors for the TURTLE structure."
  (let ((*current-turtle* (symbol-value turtle-name))
	(func (find-procedure-from-name accessor)))
    (funcall (logo-procedure-procedure func))))




;;; Turtle procedure definitions.
;;;
;;; Try to keep these ordered and grouped. The ordering is
;;; alphebetical except that the full name comes before the
;;; abbreviation.
;;;
;;;                                 procedure                            accepts
;;;                 name            symbol          args  kind    infix  list
(def-logo-primitive "ARC"           "arc"             2  :command  nil    nil)
(def-logo-primitive "ASK"           "ask"             2  :operator nil    nil)
(def-logo-primitive "BACK"          "back"            1  :command  nil    nil)
(def-logo-primitive "BK"            "back"            1  :command  nil    nil)
(def-logo-primitive "BOUNCE"        "bounce"          0  :command  nil    nil)
(def-logo-primitive "BREED"         "breed"           2  :command  nil    nil)
(def-logo-primitive "CIRCLE"        "circle"          1  :command  nil    nil)
(def-logo-primitive "CLEAN"         "clean"           0  :command  nil    nil)
(def-logo-primitive "CLEARSCREEN"   "clear-screen"    0  :command  nil    nil)
(def-logo-primitive "CS"            "clear-screen"    0  :command  nil    nil)
(def-logo-primitive "CLEARTURTLES"  "clear-turtles"   0  :command  nil    nil)
(def-logo-primitive "CLT"           "clear-turtles"   0  :command  nil    nil)
(def-logo-primitive "DRAWPOINT"     "draw-point"      0  :command  nil    nil)
(def-logo-primitive "DRP"           "draw-point"      0  :command  nil    nil)
(def-logo-primitive "FENCE"         "fence"           0  :command  nil    nil)
(def-logo-primitive "FORWARD"       "forward"         1  :command  nil    nil)
(def-logo-primitive "FD"            "forward"         1  :command  nil    nil)
(def-logo-primitive "HATCH"         "hatch"           1  :command  nil    nil)
(def-logo-primitive "HEADING"       "heading"         0  :operator nil    nil)
(def-logo-primitive "HIDETURTLE"    "hide-turtle"     0  :command  nil    nil)
(def-logo-primitive "HT"            "hide-turtle"     0  :command  nil    nil)
(def-logo-primitive "HOME"          "home"            0  :command  nil    nil)
;(def-logo-primitive "HP"            "home-position"   0  :command  nil    nil)
(def-logo-primitive "LABEL"         "label"           1  :command  nil      t)
(def-logo-primitive "LEFT"          "left-turn"       1  :command  nil    nil)
(def-logo-primitive "LT"            "left-turn"       1  :command  nil    nil)
(def-logo-primitive "PENCOLOR"      "pen-color"       0  :operator nil    nil)
(def-logo-primitive "PC"            "pen-color"       0  :operator nil    nil)
(def-logo-primitive "PENMODE"       "pen-mode"        0  :operator nil    nil)
(def-logo-primitive "PENSIZE"       "pen-size"        0  :operator nil    nil)
(def-logo-primitive "PENDOWN"       "pen-down"        0  :command  nil    nil)
(def-logo-primitive "PD"            "pen-down"        0  :command  nil    nil)
(def-logo-primitive "PENUP"         "pen-up"          0  :command  nil    nil)
(def-logo-primitive "PU"            "pen-up"          0  :command  nil    nil)
(def-logo-primitive "PENPAINT"      "pen-paint"       0  :command  nil    nil)
(def-logo-primitive "PPT"	    "pen-paint"       0  :command  nil    nil)
(def-logo-primitive "PENERASE"      "pen-erase"       0  :command  nil    nil)
(def-logo-primitive "PE"	    "pen-erase"       0  :command  nil    nil)
(def-logo-primitive "PENPOSITION"   "pen-position"    0  :operator nil    nil)
(def-logo-primitive "PENPOS"        "pen-position"    0  :operator nil    nil)
(def-logo-primitive "PENREVERSE"    "pen-reverse"     0  :command  nil    nil)
(def-logo-primitive "PX"	    "pen-reverse"     0  :command  nil    nil)
(def-logo-primitive "POSITION"      "position"        0  :operator nil    nil)
(def-logo-primitive "POS"           "position"        0  :operator nil    nil)
(def-logo-primitive "RIGHT"         "right-turn"      1  :command  nil    nil)
(def-logo-primitive "RT"            "right-turn"      1  :command  nil    nil)
(def-logo-primitive "READKEY"       "get-key"         0  :operator nil    nil)
(def-logo-primitive "RK"            "get-key"         0  :operator nil    nil)
(def-logo-primitive "SCRUNCH"       "scrunch"         0  :operator nil    nil)
(def-logo-primitive "SETFONT"       "set-font"        1  :command  nil    nil)
(def-logo-primitive "SETHEADING"    "set-heading"     1  :command  nil    nil)
(def-logo-primitive "SETH"          "set-heading"     1  :command  nil    nil)
(def-logo-primitive "SETPENCOLOR"   "set-pen-color"   1  :command  nil    nil)
(def-logo-primitive "SETPC"         "set-pen-color"   1  :command  nil    nil)
(def-logo-primitive "SETPENSIZE"    "set-pen-size"    1  :command  nil    nil)
(def-logo-primitive "SETPOS"        "set-position"    1  :command  nil    nil)
(def-logo-primitive "SETSCRUNCH"    "set-scrunch"     2  :command  nil    nil)
(def-logo-primitive "SETX"          "set-x"           1  :command  nil    nil)
(def-logo-primitive "SETY"          "set-y"           1  :command  nil    nil)
(def-logo-primitive "SETXY"         "set-xy"          2  :command  nil    nil)
(def-logo-primitive "SHOWTURTLE"    "show-turtle"     0  :command  nil    nil)
(def-logo-primitive "SHOWNP"        "shownp"          0  :operator nil    nil)
(def-logo-primitive "SHOWN?"        "shownp"          0  :operator nil    nil)
(def-logo-primitive "ST"            "show-turtle"     0  :command  nil    nil)
(def-logo-primitive "STOPTASK"      "stoptask"        1  :command  nil    nil)
(def-logo-primitive "STSK"          "stoptask"        1  :command  nil    nil)
(def-logo-primitive "TASK"          "task"            2  :command  nil    nil)
(def-logo-primitive "TELL"          "tell"            1  :command  nil    nil)
(def-logo-primitive "TELLALL"       "tellall"         1  :command  nil    nil)
(def-logo-primitive "TELLBREED"     "tellbreed"       2  :command  nil    nil)
(def-logo-primitive "TLB"           "tellbreed"       2  :command  nil    nil)
(def-logo-primitive "TOWARDS"       "towards"         1  :operator nil    nil)
(def-logo-primitive "TURTLEMODE"    "mode"            0  :operator nil    nil)
(def-logo-primitive "WINDOW"        "window"          0  :command  nil    nil)
(def-logo-primitive "WRAP"          "wrap"            0  :command  nil    nil)
(def-logo-primitive "XCOR"          "xcor"            0  :operator nil    nil)
(def-logo-primitive "YCOR"          "ycor"            0  :operator nil    nil)


#|
;;; Lisp stuff from original turtle.l file from long ago.
(defun polygon (n l) ;; draw a polygon with n edges each of length l
  (pu)
  (bk (/ l 2))
  (pd)
  (labels ((plg (n l a)
             (cond ((zerop n) T)
		   (t (fd l)
		      (rt a)
		      (plg (- n 1) l a)))))
    (plg n l (/ 360 n))))

(defun star (n l)
  (pu)
  (bk (/ l 2))
  (pd)
  ;; draw a star of n jags, size l. You'll see
  ;; that you need to give an odd amount of jags.
  (labels ((plg (n l a) 
		(cond ((zerop n) T)
		      (t (fd l)
			 (rt a)
			 (plg (- n 1) l a)))))
    (plg n l (- 180 (/ 180 n)))))
|#
