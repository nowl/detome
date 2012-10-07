(in-package :blacker)

(make-component
 "sdl receiver"
 '(:sdl-event)
 #'(lambda (message)
     (format t "received internal :sdl-event ~a~%" (payload message))
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((and (member :key-down-event (payload message))
             (eql (second (member :key (payload message)))
                  :sdl-key-escape))
        (push-quit-event)))))

(defun make-renderable-component (name meta)
  (make-component
   (concatenate 'string "renderable-image-" name)
   '(:system-render)
   #'(lambda (message)
       (let ((x (gethash "render:x" meta))
             (y (gethash "render:y" meta))
             (image (gethash "render:image" meta)))
         (loop for i below 10 do
              (loop for j below 10 do
                   (draw-image image
                               (+ (* i 25) x)
                               (+ (* j 25) y)
                               25 25
                               1 1 0)))))))

(defun load-images ()
  (clear-image-caches)
  (loop for j from 0 below 8 do
       (loop for i below 32 do
            (let ((image-name (format nil "cp437-~2,'0x" (+ i (* 32 j)))))
              (define-image image-name "/home/nowl/Desktop/Codepage-437.png" `(,(+ 8 (* 9 i))
                                                                                ,(+ 8 (* 16 j))
                                                                                9 16))))))

(defun make-system-init-component (name meta)
  (make-component
   (concatenate 'string "system-init-" name)
   '(:system-init)
   #'(lambda (message)
       (funcall (gethash "system-init:function" meta)))))

(defparameter *image-loader* (make-hash-table :test #'equal))
(setf (gethash "system-init:function" *image-loader*) #'load-images)
(make-system-init-component "image-loader" *image-loader*)

(defparameter *player* (make-hash-table :test #'equal))
(setf (gethash "render:x" *player*) 0
      (gethash "render:y" *player*) 200
      (gethash "render:image" *player*) "cp437-32")
(make-renderable-component "player" *player*)

(defun make-update-component (name meta)
  (make-component
   (concatenate 'string "update-" name)
   '(:system-update)
   #'(lambda (message)
       (funcall (gethash "update:function" meta)))))

(setf (gethash "update:function" *player*)
      #'(lambda ()
          (let ((x (gethash "render:x" *player*)))
            (setf (gethash "render:x" *player*) (+ .1 x)))))

(make-update-component "player-update" *player*)

;;(defparameter *walls* 