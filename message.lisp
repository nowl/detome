(in-package #:detome)

;;; This is some ridiculously ugly code to format the messages
;;; correctly.. I hope I never need touch this again

(defvar *message-textarea-window* (list (* 32 (nth 2 *map-window*))
					0
					(- *screen-width* (* 32 (nth 2 *map-window*)))
					*screen-height*)
  "This defines the rightmost region of the screen where text messages may appear.")

(defvar *message-area-rawtext* nil
  "A list of strings that are not necessarily formatted to appear
  onscreen. The most recent of which appears at the front.")
(defvar *message-area-strings* nil
  "A list of strings that will be rendered to the message area. There
  are special markup commands also to specify special attributes of
  the strings.")

;; Takes *message-area-rawtext* and outputs a valid
;; *message-area-strings* list for the string renderer to use
(defvar *message-area-rawtext-formatted* nil)
(defun update-message-strings (update-freq)
  ;; parse out controls and full raw string
  (let (raw-strings controls ttls)
    (loop
       with string = "" 
       with control
       with index = 0
       for ttl-and-message in *message-area-rawtext* do
	 (loop for raw-string in (second ttl-and-message) do
	      (etypecase raw-string
		(string (setf string (concatenate 'string string raw-string)
			      index (length string)))
		(cons (push (list index raw-string) control))))
	 (push string raw-strings)
	 (push (first ttl-and-message) ttls)
	 (setf controls (append controls (list control)))
	 (setf string ""
	       control nil
	       index 0))

    ;; This takes the strings in raw-strings and adds them in a
    ;; formatted way to *message-area-rawtext-formatted*
    (let ((max-chars-in-line (floor (/ (- (nth 2 *message-textarea-window*) 
					  (* 2 *message-textarea-width-offset*))
				       *primary-font-width*))))
      (flet ((build-subseqs (string)
	       (loop with index = 0 while (< index (length string)) collect
		  ;; if the next string is small enough to fit just grab
		  ;; it directly
		    (if (<= (- (length string) index) max-chars-in-line)
			(prog1
			    (subseq string index)
			  (incf index max-chars-in-line))
			;; either grab everything up to the space
			;; character that is less than the max chars in
			;; a line or else grab the max-chars in a line
			(let ((next-best-space-pos
			       (position #\  string :from-end t :start index :end (+ index max-chars-in-line) :test #'char=)))
			  (if next-best-space-pos
			      (prog1 (subseq string index next-best-space-pos)
				(setf index next-best-space-pos))
			      (prog1 (subseq string index (+ index max-chars-in-line))
				(incf index max-chars-in-line))))))))

	;; add each string that is in raw-strings
	(loop for raw-string in raw-strings 
	   for count downfrom (1- (length controls))
	   for ttl in ttls do
	     (push (list ttl
			 (build-subseqs raw-string)
			 (nreverse (nth count controls)))
		   *message-area-rawtext-formatted*))))

    ;; clear *message-area-rawtext*
    (setf *message-area-rawtext* nil)

    ;; decrement time to live and remove if dead
    (let (to-delete)
      (loop for message in *message-area-rawtext-formatted* do
	   (let* ((ttl (car message))
		  (new-ttl (- ttl update-freq)))
	     (if (minusp new-ttl)
		 (push message to-delete)
		 (rplaca message new-ttl))))
      (loop for m-to-delete in to-delete do
	   (setf *message-area-rawtext-formatted* (delete m-to-delete *message-area-rawtext-formatted* :test #'equal))))

    ;; at this point the most recent message is at the front of
    ;; *message-area-rawtext-formatted*

    (let ((start-message-index
	   ;; traverse the list and see which one we should start the
	   ;; rendering with based on the height of our text area and
	   ;; our offsets
	   (if (plusp (length *message-area-rawtext-formatted*))
	       (loop with y-offset = (+ (nth 1 *message-textarea-window*)
					*message-textarea-height-header-offset*)
		  for message-count from 0 do
		    (let* ((message (nth message-count *message-area-rawtext-formatted*))
			   (space-for-next (* (+ *primary-font-height* *message-textarea-height-offset-between*)
					      (length (second message)))))
		      (when (>= (+ space-for-next y-offset)
				(+ (nth 1 *message-textarea-window*) (nth 3 *message-textarea-window*)
				   (- *message-textarea-height-footer-offset*)))
			;; message to big to fit, return message we are on
			(return message-count))
		      (when (eq message-count (1- (length *message-area-rawtext-formatted*)))
			(return message-count))
		      ;; message can fit so add the space for it and move to the next one
		      (incf y-offset space-for-next)))
	       -1)))

      ;; dump the message strings into a flat string buffer that can
      ;; be used to render them, mark up as needed
      (setf *message-area-strings* nil)
      (labels ((append-string (obj)
	       (setf *message-area-strings*
		     (append *message-area-strings*
			     (list obj))))
	     (append-render (x y)
	       (setf *message-area-strings*
		     (append *message-area-strings*
			     (list (list :render-at x y)))))
	     (hex-string-to-color (string)
	       (let ((red (parse-integer string :start 0 :end 2 :radix 16))
		     (green (parse-integer string :start 2 :end 4 :radix 16))
		     (blue (parse-integer string :start 4 :end 6 :radix 16)))
		 (sdl:color :r red :g green :b blue)))
	     (append-color (val)
	       (setf *message-area-strings*
		     (append *message-area-strings*
			     (list (list :color (etypecase val
						  (string (hex-string-to-color val))
						  (sdl:color val))))))))
	(loop with y-offset = (+ (nth 1 *message-textarea-window*) *message-textarea-height-header-offset*)
	   with x-offset
	   for message-index downfrom start-message-index
	   with message
	   with control-index
	   with string-pos
	   with previous-chars-consumed
	   with string-index
	   while (>= message-index 0) do
	     (setf message (nth message-index *message-area-rawtext-formatted*)
		   control-index 0
		   string-index 0
		   string-pos 0
		   previous-chars-consumed 0
		   x-offset (+ (nth 0 *message-textarea-window*) *message-textarea-width-offset*))
	     (loop while (< string-index (length (second message))) do
		  (let* ((string (nth string-index (second message)))
			 (control (nth control-index (third message)))
			 (stop-pos (if control
				       (min (length string) (- (first control) previous-chars-consumed))
				       (length string))))				       
		    (cond ((eq stop-pos (length string))
			   ;; render the string as is, adjust the x-offset, and loop
			   (append-render x-offset y-offset)
			   (append-string (subseq string string-pos stop-pos))
			   (setf x-offset (+ (nth 0 *message-textarea-window*) *message-textarea-width-offset*))
			   (incf y-offset (+ *primary-font-height* *message-textarea-height-offset-between*))
			   (incf string-index)
			   (setf string-pos 0)
			   (incf previous-chars-consumed (length string)))
			  (t
			   ;; add string up to the stop point, adjust the x-offset, add the color
			   (append-render x-offset y-offset)
			   (append-string (subseq string string-pos stop-pos))
			   (incf control-index)
			   (let ((control-key (caadr control))
				 (control-val (cadadr control)))
			     (ecase control-key
			       (:color (append-color control-val))))
			   (incf x-offset (* *primary-font-width* (- stop-pos string-pos)))
			   (setf string-pos stop-pos)))))
	     (incf y-offset *message-textarea-height-offset-between-messages*))))))

(defun textarea-log (message-as-list &key (ttl *default-message-ttl-sec*))
  ;; append white color at the beginning
  ;(push (append `((:color ,sdl:*white*)) message-as-list) *message-area-rawtext*))
  (push (cons ttl (list (append `((:color ,sdl:*white*)) message-as-list))) *message-area-rawtext*))
