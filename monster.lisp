(in-package #:detome)

(defclass monster-type ()
  ((name
    :initarg :name :accessor name :type simple-string :documentation 
    "The name of this monster type")
   (image-name
    :initarg :image-name :accessor image-name :type simple-string :documentation 
    "The image name of this monster type")
   (level
    :initarg :level :accessor level :type function :documentation 
    "The monster-type's level.")
   (hp-gen
    :initarg :hp-gen :accessor hp-gen :type function :documentation
    "The monster-type's health generator..")
   (att-r-gen
    :initarg :att-r-gen :accessor att-r-gen :type function :documentation
    "The monster-type's attack rating generator.")
   (dmg-r-gen
    :initarg :dmg-r-gen :accessor dmg-r-gen :type function :documentation
    "The monster-type's damage rating generator.")
   (def-r-gen
       :initarg :def-r-gen :accessor def-r-gen :type function :documentation
       "The monster-type's defense rating generator.")
   (ai-cb
    :initarg :ai-cb :accessor ai-cb :type function :documentation
    "The monster-type's ai callback.")
   (drops
    :initarg :drops :accessor drops :type function
    :documentation
    "This function takes the current mob and returns a list of the
    drop-names from this mob.")))

(defclass monster (actor)
  ((mon-type :initarg :mon-type :accessor mon-type :type monster-type)
   (image-name :initarg :image-name :accessor image-name :type simple-string)))

(defun create-monster-from-type (x y type)
  (declare (monster-type type))
  (make-instance 'monster :x x :y y
                 :name (symbol-name (gensym (name type)))
                 :image-name (image-name type)
                 :mon-type type
                 :level (level type)
                 :hp (funcall (hp-gen type))
                 :att-r (funcall (att-r-gen type))
                 :dmg-r (funcall (dmg-r-gen type))
                 :def-r (funcall (def-r-gen type))
                 :update-cb (ai-cb type)
                 :update-cb-control '(:turns 0)))

(defun get-random-monster (x y min-level max-level)
  (let ((min-pos (position-if #'(lambda (x) (>= x min-level)) *monster-types-by-level* :key #'car))
        (max-pos (position-if #'(lambda (x) (<= x max-level)) *monster-types-by-level* :from-end t :key #'car)))
    (let* ((tmp (1+ (- max-pos min-pos)))
           (val (if (> tmp 1)
                    (+ (random tmp) min-pos)
                    min-pos)))
      (create-monster-from-type x y (cadr (nth val *monster-types-by-level*))))))

(defparameter *monster-types-by-level* nil
  "This is a sorted list of level and monster-type objects.")

(defparameter *monster-type-lookup* (make-hash-table :test 'equal))

(defmacro define-monster-type (name image-name level hp-gen att-r-gen dmg-r-gen def-r-gen ai-cb drops)
  (let ((mt (gensym)))
    `(let ((,mt (make-instance 'monster-type
                               :name ,name
                               :image-name ,image-name
                               :level ,level
                               :hp-gen ,hp-gen
                               :att-r-gen ,att-r-gen
                               :dmg-r-gen ,dmg-r-gen
                               :def-r-gen ,def-r-gen
                               :ai-cb ,ai-cb
                               :drops ,drops)))
       (setf (gethash ,name *monster-type-lookup*) ,mt)
       (setf *monster-types-by-level* (cl:remove ,name *monster-types-by-level* :key #'(lambda (x) (name (cadr x))) :test #'string=))
       (push (list ,level ,mt) *monster-types-by-level*)
       (setf *monster-types-by-level* (sort *monster-types-by-level* #'< :key #'car)))))

(defun lookup-monster-type (name)
  (gethash name *monster-type-lookup*))

(defparameter *monsters-in-level* nil)

(defun clear-monsters-from-level ()
  (loop for mon in *monsters-in-level* do
       (remove mon *play-game-state*))
  (setf *monsters-in-level* nil))

(defun monsters-at (x y)
  (loop for mon in *monsters-in-level* when (and (= (x mon) x) (= (y mon) y)) collect mon))

(defmacro clip (value a b)
  `(if (<= ,value ,a)
       ,a
       (if (>= ,value ,b)
           ,b
           ,value)))

(defun draw-monsters ()  
  (dolist (mon *monsters-in-level*)
    (multiple-value-bind (x y) (get-screen-pos-of mon)
      (let ((darken-amount (clip (- 1 (find-in-intensity-map (x mon) (y mon)))
                                 0.0 1.0)))
        (when (< darken-amount 1.0)
          (sdl:draw-surface-at-* (get-image (image-name mon) :darken darken-amount) x y))))))

(defun populate-monsters ()
  (loop for x below (+ 10 (random (array-dimension *level* 0)) ) do
       (push (get-random-monster (random (array-dimension *level* 1))
                                 (random (array-dimension *level* 0))
                                 1 10)
             *monsters-in-level*)))

(defun remove-monster (monster)
  (setf *monsters-in-level* (delete monster *monsters-in-level*))
  (remove monster *play-game-state*))

(make-object
 :name "monster garbage collector"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (dolist (mon *monsters-in-level*)
                  (when (<= (hp mon) 0)
                    (let ((drops (funcall (drops (mon-type mon)) mon)))
                      (remove-monster mon)
                      (textarea-log `((:color "00ff00") ,(name (mon-type mon)) (:color "ffffff") " dies!"))
                      (when drops
                        (dolist (drop drops)
                          (when drop
                            (textarea-log `(,(name (mon-type mon)) " drops a " ,drop))
                            (place-item drop `(:map ,(x mon) ,(y mon)))))))))))

(defun actor-not-at (x y)
  ;; test other monsters
  (dolist (actor *monsters-in-level*)
	(when (and (= (x actor) x)
			   (= (y actor) y))
	  (return-from actor-not-at nil)))
  ;; test player
  (when (and (= (x *player*) x)
			 (= (y *player*) y))
	(return-from actor-not-at nil))
  t)
