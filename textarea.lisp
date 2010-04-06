(in-package #:detome)


(defvar *message-area-rawtext* nil)
(defvar *message-area-buffer* nil)

(defun textarea-log (message-as-list 
		     &key (location-func #'(lambda (log) (push log *message-area-rawtext*))) 
		     (ttl *default-message-ttl-sec*))
  (funcall location-func (cons ttl (list (append `((:color ,sdl:*white*)) message-as-list)))))


(define-object
    :name "message textarea updater"
  :update-cb #'(lambda (obj)
                 (multiple-value-setq (*message-area-strings* *message-area-buffer*)
                   (update-message-strings
                    (second (black::update-cb-control obj))
                    *message-area-buffer*
                    :rawtext *message-area-rawtext*))
                 (setf *message-area-rawtext* nil))
  :update-cb-control '(:seconds 0.1))

(defvar *message-area-strings* nil)

(defun draw-message-textarea (strings interpolation)
  (let (pos-x pos-y (color (sdl:color :r #xff :g #xff :b #xff)))
    (loop for message in strings do
	 (etypecase message
	   (cons (ecase (car message)
		   (:render-at (setf pos-x (second message)
				     pos-y (third message)))
		   (:color (setf color (second message)))))
	   (string
	    (sdl:draw-string-solid-* message
				     pos-x
				     pos-y
				     :color color
				     :font *primary-font*))))))
