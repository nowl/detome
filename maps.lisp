(in-package #:detome)

(defparameter *level* nil)
(defparameter *level-type* nil)
(defparameter *level-width* nil)
(defparameter *level-height* nil)
(defparameter *intensity-map* nil)
(defparameter *explored-map* nil)

(defparameter *scenery-in-level* nil)

;; weather and atmosphere can make visibility much worse..
(defun apply-weather (att)
  (clip (+ att
           (second (member *weather* *weather-attens*))
           (second (member *atmosphere* *atmosphere-attens*)))
        0.0 0.99))

;; Searches all map-cells at the specified location on the map for the
;; map-cell with the largest light attenuation.
(defun attenuation-lookup (map-cells)
  (if (null map-cells)
      nil
      (let ((atts (mapcar #'(lambda (point)
                              (map-cell-attenuation (gethash point *map-cells-by-number*)))
                          map-cells)))
        (apply-weather (apply #'max atts)))))

;; hashing explored-map and intensity-map over x and y coordinates,
;; this seems to work efficiently so far..
(defmacro make-map-functions (name func)
  (let ((func1 (intern (string-upcase (format nil "clear-~a" (string func)))))
        (func2 (intern (string-upcase (format nil "find-in-~a" (string func)))))
        (func3 (intern (string-upcase (format nil "set-in-~a" (string func))))))
    `(progn
       (defun ,func1 ()
         (setf ,name (make-hash-table)))
       (defun ,func2 (x y)
         (let ((sub (gethash x ,name)))
           (if sub
               (or (gethash y sub) 0.0)
               0.0)))
       (defun ,func3 (x y val)
         (let ((sub (gethash x ,name)))
           (if sub
               (setf (gethash y sub) val)
               (progn
                 (setf (gethash x ,name) (make-hash-table))
                 (,func3 x y val))))))))

(make-map-functions *explored-map* explored-map)
(make-map-functions *intensity-map* intensity-map)

(defun get-map-points (x y)
  (ecase *level-type*
    (:predefined
     (if (or (< x 0) (< y 0) (>= x *level-width*) (>= y *level-height*))
         nil
         (aref *level* y x)))
    (:perlin
     (funcall *level* x y))))

(defun update-intensity-map (x y intensity)
  (clear-intensity-map)
  (set-in-intensity-map x y intensity)
  (let ((sights (line-of-sight x y
                               (ecase *level-type*
                                 (:predefined (1- *level-width*))
                                 (:perlin (+ x 100)))
                               (ecase *level-type*
                                 (:predefined (1- *level-height*))
                                 (:perlin (+ y 100)))
                               #'(lambda (x y)
                                   (attenuation-lookup (get-map-points x y)))
                               *light-intensity-cutoff*)))
    (loop for sight in sights do
         (destructuring-bind (x y rel-intensity) sight
           (set-in-intensity-map x y (* intensity rel-intensity))
           (set-in-explored-map x y 0.25)))))

(defun total-intensity-at-point (x y)
  (let ((map-int (find-in-intensity-map x y)))
    (if (plusp map-int)
        ;; if the LOS includes this point then only return it's intensity
        map-int
        ;; otherwise return the explored map intensity
        (find-in-explored-map x y))))

(defun darken-amount-at-point (x y)
  (clip (- 1 (total-intensity-at-point x y)) 0.0 1.0))

(defun image-from-maps (x y)
  (let ((darken-amount (darken-amount-at-point x y)))
    (let ((images (mapcar #'(lambda (mp) (map-cell-image (gethash mp *map-cells-by-number*)))
                          (get-map-points x y))))
      (loop for image in images collecting           
           (get-image (etypecase image
                        (simple-string image)
                        (function (funcall image x y)))
                      :darken darken-amount)))))

(defgeneric get-screen-pos-of (obj)
  (:documentation 
   "Returns the screen position of the given object as x and y
    values"))

(defmethod get-screen-pos-of ((obj map-object))
  (values (* (- (x obj) (first *map-window*)) 32)
          (* (- (y obj) (second *map-window*)) 32)))

(defun get-map-window-extents ()
  (let ((x-start (ecase *level-type*
                   (:predefined (max (first *map-window*) 0))
                   (:perlin (first *map-window*))))
        (y-start (ecase *level-type*
                   (:predefined (max (second *map-window*) 0))
                   (:perlin (second *map-window*))))
        (x-extent (ecase *level-type*
                    (:predefined (min *level-width*
                                      (+ (third *map-window*)
                                         (first *map-window*))))
                    (:perlin (+ (third *map-window*)
                                (first *map-window*)))))
        (y-extent (ecase *level-type*
                    (:predefined (min *level-height*
                                      (+ (fourth *map-window*)
                                         (second *map-window*))))
                    (:perlin (+ (fourth *map-window*)
                                (second *map-window*))))))
    (list x-start y-start x-extent y-extent)))


(defun draw-background ()
  (destructuring-bind (x-start y-start x-extent y-extent)
      (get-map-window-extents)
    (loop for x from x-start below x-extent do
         (loop for y from y-start below y-extent do
              (loop for image in (image-from-maps x y) do
                   (sdl:draw-surface-at-* image
                                          (* (- x x-start) 32)
                                          (* (- y y-start) 32)))))))

(defun clear-items-from-level ()
  (dolist (item *items-in-level*)
    (remove item *play-game-state*))
  (setf *items-in-level* nil))

(defun clear-scenery-from-level ()
  (dolist (scenery *scenery-in-level*)
    (remove scenery *play-game-state*))
  (setf *scenery-in-level* nil))

(defun draw-items ()
  (dolist (item *items-in-level*)
    (multiple-value-bind (x y) (get-screen-pos-of item)
      (let ((darken-amount (clip (- 1 (find-in-intensity-map (x item) (y item)))
                                 0.0 1.0)))
        (when (< darken-amount 1.0)
          (sdl:draw-surface-at-* (get-image (image-name (item-type item)) :darken darken-amount) x y))))))


;; determines if a specific spot on the map is walkable by the player
(defun walkable (x y)
  (unless (or (eq *level-type* :perlin)
              (and (eq *level-type* :predefined)
                   (>= x 0) (>= y 0) (< x *level-width*) (< y *level-height*)))
    (return-from walkable (values nil :world-extents)))
  (unless (actor-not-at x y)
    (return-from walkable (values nil :actor)))
  (loop for cell in (get-map-points x y) do
       (unless (map-cell-walkable (gethash cell *map-cells-by-number*))
         (return-from walkable (values nil :scenery))))
  t)

(make-object
 :name "environment builder"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (ecase *environment*
                  (:outside (cond ((eq *weather* :clear)
                                   (setf *weather* :dark)
                                   ;;(textarea-log '("You feel the clastrophobic oppression of darkness."))
                                   (update-intensity-map (x *player*) (y *player*) 1.0))
                                  ((eq *weather* :dark)
                                   (setf *weather* :clear)
                                   ;;(textarea-log '("The shadows become slightly less threatening as night becomes day."))
                                   (update-intensity-map (x *player*) (y *player*) 1.0))))))
 :update-cb-control `(:seconds ,*day-night-cycle-in-seconds*))
 ;;:update-cb-control :none);; `(:seconds ,*day-night-cycle-in-seconds*))
