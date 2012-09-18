(in-package :blacker)

(make-component
 "sdl receiver"
 '(:sdl-event)
 #'(lambda (message receiver)
     (declare (ignore receiver))
     (format t "received internal :sdl-event ~a~%" (payload message))
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((and (member :key-down-event (payload message))
             (eql (second (member :key (payload message)))
                  :sdl-key-escape))
        (push-quit-event))
       ((eql (car (payload message)) :video-resize-event)
        (let ((w (second (cdr (payload message))))
              (h (fourth (cdr (payload message)))))
          (setf *screen-width* w *screen-height* h)
          (setup-screen))))))
                

(defparameter *system-listener-entity*
  (make-entity '("sdl receiver")))

(make-component
 "updatable"
 '(:system-update)
 #'(lambda (message receiver)
     (format t "update is being called on the ~a object~%" receiver)
     (let ((update-func (get-meta receiver "update function")))
       (when update-func
         (funcall update-func (payload message))))))

(make-component
 "renderable-image"
 '(:system-render)
 #'(lambda (message receiver)
     (let ((x (get-meta receiver "render:screen:x"))
           (y (get-meta receiver "render:screen:y"))
           (image (get-image (get-meta receiver "render:screen:image-name"))))
           
       (draw-image-at image x y 32 32)
       ;; TODO: flush may not be needed
       (gl:flush))))

(make-component
 "system-init"
 '(:system-init)
 #'(lambda (message receiver)
     (funcall (get-meta receiver "system-init-function"))))

(defparameter *foo*
  (make-entity '("updatable")))

(defparameter *foob*
  (make-entity '("updatable")))

(defun load-images ()
  (clear-image-caches)
  (define-image "cave" "../data/tileset.png" `(1 ,(1+ (* 3 33)) 32 32)))

;;(clear-entities-from-comp "system-init")

(defparameter *image-loader*
  (make-entity '("system-init")))
(set-meta *image-loader* "system-init-function" #'load-images)

(defparameter *image-image-drawer*
  (make-entity '("renderable-image"
                 "updatable")))
(set-meta *image-image-drawer* "render:screen:x" 0)
(set-meta *image-image-drawer* "render:screen:y" 200)
(set-meta *image-image-drawer* "render:screen:image-name" "cave")
(set-meta *image-image-drawer* "update function"
          #'(lambda (tick)
              (let ((x (get-meta *image-image-drawer* "render:screen:x")))
                (set-meta *image-image-drawer* "render:screen:x" (+ 1 x)))))

;(mainloop 'test)
