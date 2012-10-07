(in-package :blacker)

(defun make-renderable-component (name meta)
  (make-component
   (concatenate 'string "renderable-image-" name)
   '(:system-render)
   #'(lambda (message)
       (let ((x (gethash "render:x" meta))
             (y (gethash "render:y" meta))
             (image (gethash "render:image" meta)))
         (draw-image image x y 13.5 24 1 1 0)))))

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
      (gethash "render:image" *player*) "cp437-40")
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

(make-component
 "sdl event receiver"
 '(:sdl-event)
 #'(lambda (message)
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((member :key-down-event (payload message))
        (case (second (member :key (payload message)))
          (:sdl-key-escape (push-quit-event))
          (:sdl-key-a (incf (gethash "render:x" *player*) -25))
          (:sdl-key-d (incf (gethash "render:x" *player*) 25))
          (:sdl-key-s (incf (gethash "render:y" *player*) 25))
          (:sdl-key-w (incf (gethash "render:y" *player*) -25)))))))


(defparameter *walls* (loop for i below 200 collect (make-hash-table :test #'equal)))

(loop for wall in *walls* do
     (setf (gethash "render:x" wall) (* 13.5 (random 60))
           (gethash "render:y" wall) (* 24 (random 30))
           (gethash "render:image" wall) "cp437-42")
     (make-renderable-component (symbol-name (gensym "wall")) wall))
