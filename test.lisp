(in-package :blacker)

(defmacro gh (ht key)
  `(gethash ,key ,ht))
(defmacro sh (ht key val)
  `(setf (gethash ,key ,ht) ,val))

(defun make-renderable-component (name meta)
  (make-component
   (concatenate 'string "renderable-image-" name)
   '(:system-render)
   #'(lambda (message)
       (let ((x (gh meta 'render-x))
             (y (gh meta 'render-y))
             (image (gh meta 'render-image)))
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
       (funcall (gh meta 'system-init-function)))))

(defparameter *image-loader* (make-hash-table :test #'equal))
(sh *image-loader* 'system-init-function #'load-images)
(make-system-init-component "image-loader" *image-loader*)

(defparameter *player* (make-hash-table :test #'equal))
(sh *player* 'render-x 0)
(sh *player* 'render-y 200)
(sh *player* 'render-image "cp437-40")
(make-renderable-component "player" *player*)

(defun make-update-component (name meta)
  (make-component
   (concatenate 'string "update-" name)
   '(:system-update)
   #'(lambda (message)
       (funcall (gh meta 'update-function)))))

(sh *player* 'update-function
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
          (:sdl-key-a (incf (gh *player* 'render-x) -25))
          (:sdl-key-d (incf (gh *player* 'render-x) 25))
          (:sdl-key-s (incf (gh *player* 'render-y) 25))
          (:sdl-key-w (incf (gh *player* 'render-y) -25)))))))


(defparameter *walls* (loop for i below 200 collect (make-hash-table)))
(defparameter *wall-names* nil)

(loop for wall in *walls* do
     (setf (gh wall 'render-x) (* 13.5 (random 60))
           (gh wall 'render-y) (* 24 (random 30))
           (gh wall 'render-image) "cp437-42")
     (let ((name (symbol-name (gensym "wall"))))
       (push name *wall-names*)
       (make-renderable-component name wall)))
