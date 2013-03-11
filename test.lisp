(in-package :detome)

(clear-components)
(clear-engine-stats)

(defparameter *view-size-multiplier* 1.25)
(defparameter *cell-dimensions* `(,(floor (* 9 *view-size-multiplier*)) 
                                   ,(floor (* 16 *view-size-multiplier*))))
(defparameter *view-cell-dimensions*
  (list (floor (/ *screen-width* (car *cell-dimensions*)))
        (floor (/ *screen-width* (cadr *cell-dimensions*)))))
(defparameter *upper-right-message-width* 20)
(defparameter *upper-right-message-x* (- (car *view-cell-dimensions*) *upper-right-message-width* 2))
(defparameter *upper-right-message-y* 1)

(defparameter *num-render-levels* 3)
(defparameter *objects-in-world* nil)
(defparameter *game-mode* 'movement)

(defparameter *quick-timed-displayer* (make-hash-table))

(sh *quick-timed-displayer*
    'message "")

(defun add-to-world-objects (object)
  (declare (hash-table object))
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
       (funcall (gh meta 'system-init-function))
       (sdl:enable-key-repeat 500 30))))

(defparameter *image-loader* (make-hash-table))
(sh *image-loader* 'system-init-function #'load-images)
(make-system-init-component "image-loader" *image-loader*)

(defparameter *player* (make-hash-table))
(sh *player* 
    'cell-x 2
    'cell-y 2
    'blocked nil
    'render-level 1
    'render-img-and-clr (name-image "player"))
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

(sh *quick-timed-displayer*
    'render-function
    #'(lambda ()
        (when (< *game-tick* (gh *quick-timed-displayer* 'stop-tick-time))
          (draw-text (gh *quick-timed-displayer* 'message)
                     *upper-right-message-x*
                     *upper-right-message-y*
                     0 1 1 :fit-to *upper-right-message-width*)))
    'display-time (/ (* 2 1000) *ms-per-update*) ; 2 seconds
    'stop-tick-time 0)

(make-component
 "quick-timed-displayer"
 '(:render :quick-message)
 #'(lambda (message)
     (ecase (type message)
       (:quick-message
        (sh *quick-timed-displayer*
            'message (payload message)
            'stop-tick-time (+ *game-tick* (gh *quick-timed-displayer* 'display-time))))
       (:render
        (funcall (gh *quick-timed-displayer* 'render-function))))))

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
|#

;;(make-update-component "player-update" *player*)

(defun show-message (where message)
  (send-message :quick-message
                (format nil "~a" message)))

(defparameter *examine-selector* (make-hash-table))

(defun enter-examine-mode ()
  (sh *examine-selector*
      'cell-x (gh *player* 'cell-x)
      'cell-y (gh *player* 'cell-y))
  (setf *game-mode* 'examine))

(defun attempt-move (x y)
  (let* ((new-x (+ x (gh *player* 'cell-x)))
         (new-y (+ y (gh *player* 'cell-y)))
         (objs (find-objects-at new-x new-y))
         (passable (loop for obj in objs collect (gh obj 'passable))))
    (cond
      ((every #'identity passable)
       ;; passable
       (sh *player*
           'cell-x new-x
           'cell-y new-y))
      (t
       ;; impassable
       (show-message 'quick-right "impassable")))))           

(defun handle-events-movement (event)
  (case event
    (:sdl-key-escape (sdl::push-quit-event))
    (:sdl-key-kp1 (attempt-move -1 1))
    (:sdl-key-kp3 (attempt-move 1 1))
    (:sdl-key-kp7 (attempt-move -1 -1))
    (:sdl-key-kp9 (attempt-move 1 -1))
    (:sdl-key-kp4 (attempt-move -1 0))
    (:sdl-key-kp6 (attempt-move 1 0))
    (:sdl-key-kp8 (attempt-move 0 -1))
    (:sdl-key-kp2 (attempt-move 0 1))
    (:sdl-key-x   (enter-examine-mode))))

(defun handle-events-examine (event)
  (case event
    (:sdl-key-escape (setf *game-mode* 'movement))
    (:sdl-key-kp1 (decf (gh *examine-selector* 'cell-x))
                  (incf (gh *examine-selector* 'cell-y)))
    (:sdl-key-kp3 (incf (gh *examine-selector* 'cell-x))
                  (incf (gh *examine-selector* 'cell-y)))
    (:sdl-key-kp7 (decf (gh *examine-selector* 'cell-x))
                  (decf (gh *examine-selector* 'cell-y)))
    (:sdl-key-kp9 (incf (gh *examine-selector* 'cell-x))
                  (decf (gh *examine-selector* 'cell-y)))
    (:sdl-key-kp4 (decf (gh *examine-selector* 'cell-x)))
    (:sdl-key-kp6 (incf (gh *examine-selector* 'cell-x)))
    (:sdl-key-kp8 (decf (gh *examine-selector* 'cell-y)))
    (:sdl-key-kp2 (incf (gh *examine-selector* 'cell-y))))
  (let ((objs (find-objects-at (gh *examine-selector* 'cell-x) (gh *examine-selector* 'cell-y))))
    (sh *examine-selector* 'objects
        objs)))

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
        (let ((event (second (member :key (payload message)))))
          (ecase *game-mode*
            (movement (handle-events-movement event))
            (examine (handle-events-examine event)))))
       #+debug
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
                 
(defparameter map-data (mid-displace (first *view-cell-dimensions*) (second *view-cell-dimensions*) :roughness 32.0))

(defparameter *walls* nil)
(defparameter *wall-names* nil)

(defmacro make-wall-object (desc name passable)
    `(let ((wall (make-hash-table)))
       (sh wall
           'passable ,passable
           'cell-x j
           'cell-y i
           'render-level 0
           'short-desc ,desc
           'render-img-and-clr (name-image ,name))
       (push wall *walls*)
       (let ((name (symbol-name (gensym ,name))))
         (push name *wall-names*)
         (make-renderable-component name wall)
         (add-to-world-objects wall))))

(loop for i below (array-dimension map-data 0) do
     (loop for j below (array-dimension map-data 1) do
          (cond ((> (aref map-data i j) 0.75)
                 (make-wall-object "a wall" "wall" nil))
                ((> (aref map-data i j) 0.6)
                 (make-wall-object "water" "water" t))
                (t
                 (make-wall-object "sand" "sand" t)))))
                
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

(make-component
 "examine-box-selector"
 '(:render)
 #'(lambda (message)
     (when (eq *game-mode* 'examine)
       (let ((x (gh *examine-selector* 'cell-x))
             (y (gh *examine-selector* 'cell-y)))
         (draw-selector x y 4)
         (let ((objs (gh *examine-selector* 'objects)))
           (when objs
             (let ((message (format nil "You see ~a" (gh (first objs) 'short-desc))))
               (draw-text message
                          *upper-right-message-x*
                          *upper-right-message-y*
                          
                          ;;(1+ (- (first *view-cell-dimensions*) *hud-cell-width*)) 1 
                          0 1 0 :fit-to *upper-right-message-width*))))))))                          
                          ;;(- *hud-cell-width* 2)))))))))

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
