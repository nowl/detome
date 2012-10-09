(in-package :blacker)

(clear-components)

(defmacro gh (ht key)
  `(gethash ,key ,ht))
(defmacro sh (ht key val)
  `(setf (gethash ,key ,ht) ,val))

(defparameter *num-render-levels* 2)

(make-component 
 "main-render"
 '(:system-render)
 #'(lambda (message)
     (let ((interpolation (payload message)))
       (loop for render-level below *num-render-levels* do
       (send-message :render (list render-level interpolation) :async)))))

(defun make-renderable-component (name meta)
  (make-component
   (concatenate 'string "renderable-image-" name)
   '(:render)
   #'(lambda (message)
       (let ((x (gh meta 'render-x))
             (y (gh meta 'render-y))
             (image (gh meta 'render-image))
             (level (gh meta 'render-level)))
         (when (= level (car (payload message)))
           (draw-image image x y 13.5 24 1 1 0))))))

(defun load-images ()
  (clear-image-caches)
  (loop for j from 0 below 8 do
       (loop for i below 32 do
            (let ((image-name (format nil "cp437-~2,'0x" (+ i (* 32 j)))))
              (define-image image-name "../Codepage-437-solid.png" `(,(+ 8 (* 9 i))
                                                                   ,(+ 8 (* 16 j))
                                                                   9 16))))))

(defun make-system-init-component (name meta)
  (make-component
   (concatenate 'string "system-init-" name)
   '(:system-init)
   #'(lambda (message)
       (funcall (gh meta 'system-init-function)))))

(defparameter *image-loader* (make-hash-table))
(sh *image-loader* 'system-init-function #'load-images)
(make-system-init-component "image-loader" *image-loader*)

(defparameter *player* (make-hash-table))
(sh *player* 'render-x 0)
(sh *player* 'render-y 200)
(sh *player* 'render-level 1)
(sh *player* 'render-image "cp437-40")
(make-renderable-component "player" *player*)

(defun make-timed-update (num-ticks name meta)
  (let ((timed-symbol (gensym (concatenate 'string "timer-" name))))
    (sh meta timed-symbol (+ num-ticks *game-tick*))
    (make-component
     (concatenate 'string "update-timed-" name)
     '(:system-update)
     #'(lambda (message)
         (let ((time (gh meta timed-symbol)))
           (when (>= *game-tick* time)
             (funcall (gh meta 'update-function))
             (sh meta timed-symbol (+ num-ticks time))))))))

(defparameter *fps-displayer* (make-hash-table))
(sh *fps-displayer* 'update-function
    #'(lambda ()
        (format t "fps: ~a~%" *fps-counter*)))
(make-timed-update 40 "fps-displayer" *fps-displayer*)

(defun make-update-component (name meta)
  (make-component
   (concatenate 'string "update-" name)
   '(:system-update)
   #'(lambda (message)
       (funcall (gh meta 'update-function)))))

(sh *player* 'update-function
    #'(lambda ()
        (let ((x (gh *player* 'render-x)))
          (sh *player* 'render-x (+ .1 x)))))

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
           (gh wall 'render-level) 0
           (gh wall 'render-image) "cp437-42")
     (let ((name (symbol-name (gensym "wall"))))
       (push name *wall-names*)
       (make-renderable-component name wall)))
