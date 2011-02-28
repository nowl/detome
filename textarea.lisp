(in-package #:detome)


(defparameter *message-area-rawtext* nil)
(defparameter *message-area-buffer* nil)

(defun textarea-log (message-as-list 
		     &key (location-func #'(lambda (log) (push log *message-area-rawtext*))) 
		     (ttl *default-message-ttl-sec*))
  (when (not *draw-textarea-window*)
    ;; turn off message exclamation and the remover
    (setf *draw-message-exclamation*
          t
          (update-cb-control (lookup-by-name "message notification remover"))
          `(:seconds ,*seconds-to-remove-message-notification*)
          (update-cb-control (lookup-by-name "message notification renderer"))
          '(:seconds 0.5)))  
  (funcall location-func (cons ttl (list (append `((:color ,sdl:*white*)) message-as-list)))))


(make-object
 :name "message textarea updater"
 :update-cb #'(lambda (obj)
                (multiple-value-setq (*message-area-strings* *message-area-buffer*)
                  (update-message-strings
                   (second (black::update-cb-control obj))
                   *message-area-buffer*
                   :rawtext *message-area-rawtext*))
                (setf *message-area-rawtext* nil))
 :update-cb-control '(:seconds 0.1))

(defparameter *message-area-strings* nil)

(defun draw-message-textarea (strings)
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
				     :font *primary-font*))
       (null nil)))))

;; this draws the message notification object on the map if necessary
(make-object
 :name "message notification renderer"
 :render-level "notifications"
 :update-cb #'(lambda (obj)
                (when *draw-message-exclamation*
                  (set-meta (:cycle obj)
                    (not (get-meta :cycle obj)))))
 :update-cb-control '(:seconds 0.5)
 :render-cb #'(lambda (obj)
                (when (and *draw-message-exclamation* (get-meta :cycle obj))
                  (sdl:draw-surface-at-* (get-image "message-exclamation")
                                         (- *screen-width* (+ 32 16))
                                         16))))

;; this will turn off the message notification if it's on too long
(make-object
 :name "message notification remover"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (setf *draw-message-exclamation* nil))
 :update-cb-control :none)
