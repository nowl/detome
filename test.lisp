(in-package :detome)

(clear-components)
(clear-engine-stats)

(defparameter *view-size-multiplier* 1.25)
(defparameter *cell-dimensions* `(,(floor (* 9 *view-size-multiplier*)) 
                                   ,(floor (* 16 *view-size-multiplier*))))
(defparameter *hud-cell-width* 20)
(defparameter *view-cell-dimensions*
  (list (- (floor (/ *screen-width* (car *cell-dimensions*))) *hud-cell-width*)
        (floor (/ *screen-width* (cadr *cell-dimensions*)))))
(defparameter *num-render-levels* 3)
(defparameter *objects-in-world* nil)

(defun add-to-world-objects (object)
  (push object *objects-in-world*))

(defun find-objects-at (x y)
  (loop for obj in *objects-in-world* when
       (with-ghs (cell-x cell-y) obj
         (and (= x cell-x) (= y cell-y)))
     collect obj))

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
         (with-ghs (cell-x cell-y render-img-and-clr render-level) meta
           (when (= render-level (car (payload message)))
             (destructuring-bind (image (r g b)) render-img-and-clr
               (draw-image image
                           (* cw cell-x)
                           (* ch cell-y)
                           cw ch r g b))))))))

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
       (declare (ignore message))
       (funcall (gh meta 'system-init-function)))))

(defparameter *image-loader* (make-hash-table))
(sh *image-loader* 'system-init-function #'load-images)
(make-system-init-component "image-loader" *image-loader*)

(defparameter *player* (make-hash-table))
(sh *player* 
    'cell-x 2
    'cell-y 2
    'blocked nil
    'render-level 1
    'render-img-and-clr (name-image "player")
    'box-selector '(0 0)
    'hover-object nil)
(make-renderable-component "player" *player*)
(add-to-world-objects *player*)

;; time can be a number of ticks or a list containing (:seconds time)
;; or (:ticks time)
(defun make-timed-update (time name meta)
  (let ((timed-symbol (gensym (concatenate 'string "timer-" name)))
        (num-ticks (etypecase time
                     (fixnum time)
                     (list (ecase (first time)
                             (:ticks (second time))
                             (:seconds (/ (* (second time) 1000)
                                          *ms-per-update* )))))))
    (sh meta timed-symbol (+ num-ticks *game-tick*))
    (make-component
     (concatenate 'string "update-timed-" name)
     '(:system-update)
     #'(lambda (message)
         (declare (ignore message))
         (let ((time (gh meta timed-symbol)))
           (when (>= *game-tick* time)
             (funcall (gh meta 'update-function))
             (sh meta timed-symbol (+ num-ticks time))))
         nil))))

(defparameter *fps-displayer* (make-hash-table))
(sh *fps-displayer* 'update-function
    #'(lambda ()
        (format t "fps: ~a~%" *fps-counter*)))
(make-timed-update '(:seconds 2) "fps-displayer" *fps-displayer*)

(defun make-update-component (name meta)
  (make-component
   (concatenate 'string "update-" name)
   '(:system-update)
   #'(lambda (message)
       (declare (ignore message))
       (funcall (gh meta 'update-function)))))

#|
(sh *player* 'update-function
    #'(lambda ()
        (let ((x (gh *player* 'cell-x)))
          (sh *player* 'cell-x (+ .2 x)))
        nil))

(make-update-component "player-update" *player*)
|#

(defmacro move-player* (move-attempt-x move-attempt-y)
  (let ((move-x (gensym))
        (move-y (gensym)))
    `(let ((,move-x ,move-attempt-x)
           (,move-y ,move-attempt-y))
       (sh *player* 'blocked nil)
       (send-message :movement
                     (list ,move-x ,move-y "player")
                     :async)
       (when (not (gh *player* 'blocked))
         (sh *player* 
             'cell-x ,move-x
             'cell-y ,move-y)))))
  
(make-component
 "sdl event receiver"
 '(:sdl-event)
 #'(lambda (message)
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((member :key-down-event (payload message))
        (case (second (member :key (payload message)))
          (:sdl-key-escape (sdl::push-quit-event))
          (:sdl-key-a (move-player* (1- (gh *player* 'cell-x)) (gh *player* 'cell-y)))
          (:sdl-key-d (move-player* (1+ (gh *player* 'cell-x)) (gh *player* 'cell-y)))
          (:sdl-key-s (move-player* (gh *player* 'cell-x) (1+ (gh *player* 'cell-y))))
          (:sdl-key-w (move-player* (gh *player* 'cell-x) (1- (gh *player* 'cell-y))))))
       ((member :mouse-motion-event (payload message))
        (let ((x (second (member :x (payload message))))
              (y (second (member :y (payload message)))))
          (destructuring-bind (cw ch) *cell-dimensions*
            (let ((cell-x (floor (/ x cw)))
                  (cell-y (floor (/ y ch))))
            (sh *player* 'box-selector (list cell-x cell-y))
            (let ((objs (find-objects-at cell-x cell-y)))
              (sh *player* 'hover-object 
                  (when objs
                    (format nil "You see ~a" (gh (first objs) 'short-desc))))))))))))
                 
(defparameter *walls* (loop for i below 500 collect (make-hash-table)))
(defparameter *wall-names* nil)

;; build walls
(loop for wall in *walls* do
     (let ((typ (random 2)))
       (sh wall
           'cell-x (random (first *view-cell-dimensions*))
           'cell-y (random (second *view-cell-dimensions*))
           'render-level 0
           'short-desc (if (= 0 typ) "water" "a wall")
           'render-img-and-clr (name-image (if (= 0 typ)
                                               "water"
                                               "wall"))))
     (let ((name (symbol-name (gensym "wall"))))
       (push name *wall-names*)
       (make-renderable-component name wall)
       (add-to-world-objects wall)))

;; add movement receiver
(make-component
 "movement check"
 '(:movement)
 #'(lambda (message)
     (destructuring-bind (x y sender) (payload message)
       (loop for wall in *walls* do
            (when (and (eql (gh wall 'cell-x) x)
                       (eql (gh wall 'cell-y) y)
                       (equal sender "player"))
              (sh *player* 'hover-object 
                  (format nil "Blocked by ~a" (gh wall 'short-desc)))
              (send-message :player-blocked nil :async)
              t)))))

;; add player blocked reciever
(make-component
 "blocked check"
 '(:player-blocked)
 #'(lambda (message)
     (declare (ignore message))
     (sh *player* 'blocked t)))

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
           (draw-selector x y 4)
           (let ((hover-obj (gh *player* 'hover-object)))
             (when hover-obj
               (draw-text hover-obj
                          (1+ (first *view-cell-dimensions*)) 1 0 1 0 :fit-to (- *hud-cell-width* 2)))))))))

#|
(defparameter *dungeon*
  (create-dungeon :num-x-sections 100 :num-y-sections 100
                  :section-width 10 :section-height 10
                  :min-room-width 3 :max-room-width 9
                  :min-room-height 3 :max-room-height 9
                  :room-prob 0.4
                  :min-passage-length 2 :max-passage-length 10
                  :min-num-exits 1 :max-num-exits 4))
|#

;(mainloop)