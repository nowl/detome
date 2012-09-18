(in-package :blacker)

(make-component
 "sdl receiver"
 '(:sdl-event)
 #'(lambda (message receiver)
     (declare (ignore receiver))
     ;;(format t "received internal :sdl-event ~a~%" (payload message))
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
          (setup-screen)))
       ((and (eql (car (payload message)) :mouse-button-up-event)
             (= (second (member :button (payload message))) 5))
        (destructuring-bind (left right bottom top) *world-view*
          (setf *world-view* (list left (* right 1.01) (* bottom 1.01) top))
          (set-world-view)))
       ((and (eql (car (payload message)) :mouse-button-up-event)
             (= (second (member :button (payload message))) 4))
        (destructuring-bind (left right bottom top) *world-view*
          (setf *world-view* (list left (* right 0.99) (* bottom 0.99) top))
          (set-world-view))))))
                

(defparameter *system-listener-entity*
  (make-entity '("sdl receiver")))

(make-component
 "updatable"
 '(:system-update)
 #'(lambda (message receiver)
     ;;(format t "update is being called on the ~a object~%" receiver)
     (let ((update-func (get-meta receiver "update function")))
       (when update-func
         (funcall update-func (payload message))))))

(make-component
 "renderable-image"
 '(:system-render)
 #'(lambda (message receiver)
     (let ((i (get-meta receiver "render:screen:x"))
           (j (get-meta receiver "render:screen:y"))
           (image (get-image (get-meta receiver "render:screen:image-name"))))
           
       (loop for x below 26 do
            (loop for y below 24 do
                 (draw-image-at image (+ i x) (+ j y) 1 1))))))

(make-component
 "system-init"
 '(:system-init)
 #'(lambda (message receiver)
     (funcall (get-meta receiver "system-init-function"))))

(defparameter *foo*
  (make-entity '("updatable")))
;;(set-meta *foo* "update function"
;;          #'(lambda (tick)
;;              (destructuring-bind (left right bottom top) *world-view*
;;                (setf *world-view* (list left (* right 1.01) (* bottom 1.01) top))
;;                (set-world-view))))

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
(set-meta *image-image-drawer* "render:screen:y" 0)
(set-meta *image-image-drawer* "render:screen:image-name" "cave")
(set-meta *image-image-drawer* "update function"
          #'(lambda (tick)
              (let ((x (get-meta *image-image-drawer* "render:screen:x")))
                (set-meta *image-image-drawer* "render:screen:x" (+ 0 x)))
              nil))

;(mainloop 'test)
