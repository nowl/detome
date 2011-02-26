(in-package #:detome)

(defparameter *level* nil)
(defparameter *level-width* nil)
(defparameter *level-height* nil)
(defparameter *intensity-map* nil)
(defparameter *explored-map* nil)

;; Searches all map-cells at the specified location on the map for the
;; map-cell with the largest light attenuation.
(defun attenuation-lookup (x y map)
  (let* ((map-points (aref map y x))
         (atts (mapcar #'(lambda (point)
                           (map-cell-attenuation (gethash point *map-cells-by-number*)))
                       map-points)))
    (apply #'max (mapcar #'(lambda (att)
                             (etypecase att
                               (float att)
                               (cons (let ((res (member *weather* att)))
                                       (if res
                                           (second res)
                                           (first att))))))
                         atts))))

(defmacro make-map-functions (name func)
  (let ((func1 (intern (string-upcase (format nil "clear-~a" (string func)))))
        (func2 (intern (string-upcase (format nil "find-in-~a" (string func)))))
        (func3 (intern (string-upcase (format nil "set-in-~a" (string func))))))
    `(progn
       (defun ,func1 ()
         (setf ,name nil))
       (defun ,func2 (x y)
         (let ((res (assoc (list x y) ,name :test #'equal)))
           (if res
               (cadr res)
               0.0)))
       (defun ,func3 (x y val)
         (let ((elem (assoc (list x y) ,name :test #'equal)))
           (if elem
               (rplacd (assoc (list x y) ,name :test #'equal) (list val))
               (push `((,x ,y) ,val) ,name)))))))

(make-map-functions *explored-map* explored-map)
(make-map-functions *intensity-map* intensity-map)

(defun update-intensity-map (x y intensity)
  (clear-intensity-map)
  (set-in-intensity-map x y intensity)
  (let ((sights (line-of-sight x y
                               *level-width*
                               *level-height*
                               #'(lambda (x y) (funcall #'detome::attenuation-lookup x y detome::*level*)) 
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
    ;; only return the first (the actual "map" one, the rest are
    ;; scenery
    (let* ((map-point (car (aref *level* y x)))
           (images (map-cell-image (gethash map-point *map-cells-by-number*))))
      (etypecase images
        (cons (loop for image in images collecting
                   (get-image image :darken darken-amount)))
        (string (list (get-image images :darken darken-amount)))))))

(defgeneric get-screen-pos-of (obj)
  (:documentation 
   "Returns the screen position of the given object as x and y
    values"))

(defmethod get-screen-pos-of ((obj actor))
  (values (* (- (x obj) (first *map-window*)) 32)
          (* (- (y obj) (second *map-window*)) 32)))


(defun draw-background ()
  (loop for x from (max (first *map-window*) 0) below (min *level-width*
                                                           (+ (third *map-window*)
                                                              (first *map-window*))) do
       (loop for y from (max (second *map-window*) 0) below (min *level-height*
                                                                 (+ (fourth *map-window*)
                                                                    (second *map-window*))) do
            (let ((images (image-from-maps x y)))
              (loop for image in images do
                   (sdl:draw-surface-at-* image
                                          (* (- x (first *map-window*)) 32)
                                          (* (- y (second *map-window*)) 32)))))))

(defun scenery-renderer (obj)
  (let ((image (get-meta :image obj))
        (x (get-meta :x obj))
        (y (get-meta :y obj)))
    (sdl:draw-surface-at-* (get-image image :darken (darken-amount-at-point x y))
                           (* (- x (first *map-window*)) 32)
                           (* (- y (second *map-window*)) 32))))

;; determines if a specific spot on the map is walkable by the player
(defun walkable (x y)
  (unless (and (>= x 0) (>= y 0) (< x *level-width*) (< y *level-height*))
    (return-from walkable (values nil :world-extents)))
  (unless (actor-not-at x y)
    (return-from walkable (values nil :actor)))
  (let* ((map-point (aref *level* y x)))
    (loop for cell in map-point do
         (unless (map-cell-walkable (gethash cell *map-cells-by-number*))
           (return-from walkable (values nil :scenery)))))
  t)

(make-object
 :name "weather builder"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (ecase *environment*
                  (:outside (cond ((eq *weather* :clear)
                                   (setf *weather* :dark)
                                   (textarea-log '("You feel the clastrophobic oppression of darkness."))
                                   (update-intensity-map (x *player*) (y *player*) 1.0))
                                  ((eq *weather* :dark)
                                   (setf *weather* :clear)
                                   (textarea-log '("The shadows become slightly less threatening as night becomes day."))
                                   (update-intensity-map (x *player*) (y *player*) 1.0))))))
 :update-cb-control :none);; `(:seconds ,*day-night-cycle-in-seconds*))
