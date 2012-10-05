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
           (c (get-meta receiver "render:color:red")))
       (loop for i below 10 do
            (loop for j below 10 do
                 (draw-image (get-meta receiver "render:screen:image-name")
                             (+ (* i 25) x)
                             (+ (* j 25) y)
                             25 25
                             c (- 1 c) 0))))))

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
  (loop for j from 0 below 8 do
       (loop for i below 32 do
            (let ((image-name (format nil "cp437-~2,'0x" (+ i (* 32 j)))))
              (define-image image-name "/home/nowl/Desktop/Codepage-437.png" `(,(+ 8 (* 9 i))
                                                                                ,(+ 8 (* 16 j))
                                                                                9 16))))))

;;(clear-entities-from-comp "system-init")

(defparameter *image-loader*
  (make-entity '("system-init")))
(set-meta *image-loader* "system-init-function" #'load-images)

(defparameter *image-image-drawer*
  (make-entity '("renderable-image"
                 "updatable")))
(set-meta *image-image-drawer* "render:screen:x" 0)
(set-meta *image-image-drawer* "render:screen:y" 100)
(set-meta *image-image-drawer* "render:color:red" 1)
(set-meta *image-image-drawer* "render:screen:image-name" "cp437-32")
(set-meta *image-image-drawer* "update function"
          #'(lambda (tick)
              (let ((x (get-meta *image-image-drawer* "render:screen:x"))
                    (c (get-meta *image-image-drawer* "render:color:red")))
                (set-meta *image-image-drawer* "render:screen:x" (+ .03 x))
                (set-meta *image-image-drawer* "render:color:red" (- c 0.002)))))

;(mainloop 'test)