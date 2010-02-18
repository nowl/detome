(in-package #:detome)

;;; This is some ridiculously ugly code to format the messages
;;; correctly.. I hope I never need touch this again

(defun hex-string-to-color (string)
  (let ((red (parse-integer string :start 0 :end 2 :radix 16))
	(green (parse-integer string :start 2 :end 4 :radix 16))
	(blue (parse-integer string :start 4 :end 6 :radix 16)))
    (sdl:color :r red :g green :b blue)))

(defun update-message-strings (update-freq formatted-buffer &key rawtext 
			       (message-textarea-window *default-message-textarea-window*))
  "Takes some rawtext and outputs a valid string buffer for use with
  the string renderer and also a valid formatted-buffer for use with
  subsequent calls to update-message-strings.

  Inputs: 
    
    update-freq - A float representing how much time to remove from
        the time-to-live of the messages in the formatted-buffer.

    rawtext - A list of strings with markup that should appear
        onscreen. The most recent of which appears at the front.

    formatted-buffer - An external buffer that is returned by this
        function and should be provided to subsequent calls to the
        function. It maintains a list of the formatted strings and
        also the time-to-live for each string and serves as an
        intermediate point between rawtext and the final renderable
        string buffer.

    message-textarea-window - A rectangle (x y w h) in screen pixels
        representing where the text should be fitted.
   
  Outputs:

    Multiple values. The first of which is a list of strings that can
    be rendered to the message area. There are special markup commands
    also to specify special attributes of the strings. The second
    value is the formatted-buffer object that should be kept and
    passed back into this function on subsequent calls.
  "
  (declare (single-float update-freq)
	   (list formatted-buffer rawtext message-textarea-window))

  ;; parse out controls and full raw string
  (let (raw-strings controls ttls)
    (loop
       with string = "" 
       with control
       with index = 0
       for ttl-and-message in rawtext do
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
    ;; formatted way to formatted-buffer
    (let ((max-chars-in-line (floor (/ (- (nth 2 message-textarea-window) 
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
		   formatted-buffer))))

    ;; decrement time to live and remove if dead
    (let (to-delete)
      (loop for message in formatted-buffer do
	   (let* ((ttl (car message))
		  (new-ttl (- ttl update-freq)))
	     (if (minusp new-ttl)
		 (push message to-delete)
		 (rplaca message new-ttl))))
      (loop for m-to-delete in to-delete do
	   (setf formatted-buffer (delete m-to-delete formatted-buffer :test #'equal))))

    ;; at this point the most recent message is at the front of
    ;; formatted-buffer

    (let ((start-message-index
	   ;; traverse the list and see which one we should start the
	   ;; rendering with based on the height of our text area and
	   ;; our offsets
	   (if (plusp (length formatted-buffer))
	       (loop with y-offset = (+ (nth 1 message-textarea-window)
					*message-textarea-height-header-offset*)
		  for message-count from 0 do
		    (let* ((message (nth message-count formatted-buffer))
			   (space-for-next (* (+ *primary-font-height* *message-textarea-height-offset-between*)
					      (length (second message)))))
		      (when (>= (+ space-for-next y-offset)
				(+ (nth 1 message-textarea-window) (nth 3 message-textarea-window)
				   (- *message-textarea-height-footer-offset*)))
			;; message to big to fit, return message we are on
			(return message-count))
		      (when (eq message-count (1- (length formatted-buffer)))
			(return message-count))
		      ;; message can fit so add the space for it and move to the next one
		      (incf y-offset space-for-next)))
	       -1)))

      ;; dump the message strings into a flat string buffer that can
      ;; be used to render them, mark up as needed
      (let (message-area-strings)
	(labels ((append-string (obj)
		   (setf message-area-strings
			 (append message-area-strings
				 (list obj))))
		 (append-render (x y)
		   (setf message-area-strings
			 (append message-area-strings
				 (list (list :render-at x y)))))
		 (append-color (val)
		   (setf message-area-strings
			 (append message-area-strings
				 (list (list :color (etypecase val
						      (string (hex-string-to-color val))
						      (sdl:color val))))))))
	  (loop with y-offset = (+ (nth 1 message-textarea-window) *message-textarea-height-header-offset*)
	     with x-offset
	     for message-index downfrom start-message-index
	     with message
	     with control-index
	     with string-pos
	     with previous-chars-consumed
	     with string-index
	     while (>= message-index 0) do
	       (setf message (nth message-index formatted-buffer)
		     control-index 0
		     string-index 0
		     string-pos 0
		     previous-chars-consumed 0
		     x-offset (+ (nth 0 message-textarea-window) *message-textarea-width-offset*))
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
			     (setf x-offset (+ (nth 0 message-textarea-window) *message-textarea-width-offset*))
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
	       (incf y-offset *message-textarea-height-offset-between-messages*)))
	(values message-area-strings formatted-buffer)))))
