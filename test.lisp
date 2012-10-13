(in-package :detome)

(clear-components)
(clear-engine-stats)

(defparameter *cell-dimensions* `(,(floor (* 9 1.25)) ,(floor (* 16 1.25))))
(defparameter *hud-cell-width* 20)
(defparameter *view-cell-width* (- (floor (/ *screen-width* (car *cell-dimensions*))) *hud-cell-width*))
(defparameter *view-cell-height* (floor (/ *screen-width* (cadr *cell-dimensions*))))

(defparameter *num-render-levels* 3)

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
       (destructuring-bind (cw ch) *cell-dimensions*
         (let ((x (* cw (gh meta 'cell-x)))
               (y (* ch (gh meta 'cell-y)))
               (image-and-color (gh meta 'render-img-and-clr))
               (level (gh meta 'render-level)))
           (when (= level (car (payload message)))
             (destructuring-bind (image (r g b)) image-and-color
               (draw-image image x y cw ch r g b))))))))

(defun load-images ()
  (clear-image-caches)
  (loop for j from 0 below 8 do
       (loop for i below 32 do
            (let ((image-name (format nil "cp437-~2,'0x" (+ i (* 32 j)))))
              (define-image image-name "Codepage-437-solid.png" `(,(+ 8 (* 9 i))
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
(sh *player* 
    'cell-x 2
    'cell-y 2
    'render-level 1
    'render-img-and-clr (name-image "player")
    'box-selector '(0 0))
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
             (sh meta timed-symbol (+ num-ticks time))))
         nil))))

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
        (let ((x (gh *player* 'cell-x)))
          (sh *player* 'cell-x (+ .2 x)))
        nil))

(make-update-component "player-update" *player*)

(make-component
 "sdl event receiver"
 '(:sdl-event)
 #'(lambda (message)
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((member :key-down-event (payload message))
        (case (second (member :key (payload message)))
          (:sdl-key-escape (sdl::push-quit-event))
          (:sdl-key-a (incf (gh *player* 'cell-x) -25))
          (:sdl-key-d (incf (gh *player* 'cell-x) 25))
          (:sdl-key-s (incf (gh *player* 'cell-y) 25))
          (:sdl-key-w (incf (gh *player* 'cell-y) -25))))
       ((member :mouse-motion-event (payload message))
        (let ((x (second (member :x (payload message))))
              (y (second (member :y (payload message)))))
          (destructuring-bind (cw ch) *cell-dimensions*
            (sh *player* 'box-selector (list (floor (/ x cw)) (floor (/ y ch))))))))))
                 
(defparameter *walls* (loop for i below 2000 collect (make-hash-table)))
(defparameter *wall-names* nil)

;; build walls
(loop for wall in *walls* do
     (destructuring-bind (cw ch) *cell-dimensions*
       (sh wall
           'cell-x (random *view-cell-width*)
           'cell-y (random *view-cell-height*)
           'render-level 0
           'render-img-and-clr (name-image "water")))
     (let ((name (symbol-name (gensym "wall"))))
       (push name *wall-names*)
       (make-renderable-component name wall)))

(defun draw-box (x y)
  (destructuring-bind (cw ch) *cell-dimensions*
    (let ((lx (1- (* x cw)))
          (rx (1+ (+ cw (* x cw))))
          (by (1+ (+ ch (* y ch))))
          (ty (1- (* y ch))))
      (draw-line lx ty rx ty 1 1 1)
      (draw-line rx ty rx by 1 1 1)
      (draw-line rx by lx by 1 1 1)
      (draw-line lx by lx ty 1 1 1))))

(defun draw-selector (x y divider)
  (destructuring-bind (cw ch) *cell-dimensions*
    (let ((lx (1- (* x cw)))
          (rx (1+ (+ cw (* x cw))))
          (by (1+ (+ ch (* y ch))))
          (ty (1- (* y ch))))
      (draw-line lx ty (+ lx (/ (- rx lx) divider)) ty 1 1 1)
      (draw-line (- rx (/ (- rx lx) divider)) ty rx ty 1 1 1)
      (draw-line rx ty rx (+ ty (/ (- by ty) divider)) 1 1 1)
      (draw-line rx (- by (/ (- by ty) divider)) rx by 1 1 1)
      (draw-line lx by (+ lx (/ (- rx lx) divider)) by 1 1 1)
      (draw-line (- rx (/ (- rx lx) divider)) by rx by 1 1 1)
      (draw-line lx ty lx (+ ty (/ (- by ty) divider)) 1 1 1)
      (draw-line lx (- by (/ (- by ty) divider)) lx by 1 1 1))))
  
(defparameter *box-selector-comp*
  (make-component
   (concatenate 'string "box-selector")
   '(:render)
   #'(lambda (message)
       (destructuring-bind (x y) (gh *player* 'box-selector)
         (when (and (= 2 (car (payload message)))
                    (< x (- (/ *screen-width* (car *cell-dimensions*)) *hud-cell-width*)))
           (draw-selector x y 4))))))

;(mainloop)