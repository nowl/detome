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
        (push-quit-event)))))

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
           
       (gl:bind-texture :texture-2d image)
       (gl:color 1 1 1)
       (gl:enable :texture-2d)
       ;;(gl:enable :blend)
       ;;(gl:blend-func :src-alpha :one-minus-src-alpha)

       (gl:with-primitive :quads
         (gl:tex-coord 0 0)
         (gl:vertex x 0.0)
         (gl:tex-coord 1 0)
         (gl:vertex (+ x 100.0) 0.0)
         (gl:tex-coord 1 1)
         (gl:vertex (+ x 100.0) 100.0)
         (gl:tex-coord 0 1)
         (gl:vertex x 100.0))
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
                (set-meta *image-image-drawer* "render:screen:x" (+ 0.1 x)))))

;(mainloop 'test)