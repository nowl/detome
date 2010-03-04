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
    "The monster-type's ai callback.")))	  

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

(defvar *monster-types-by-level* nil
  "This is a sorted list of level and monster-type objects.")

(defmacro define-monster-type (name image-name level hp-gen att-r-gen dmg-r-gen def-r-gen ai-cb)
  (let ((mt (gensym)))
    `(let ((,mt (make-instance 'monster-type
                               :name ,name
                               :image-name ,image-name
                               :level ,level
                               :hp-gen ,hp-gen
                               :att-r-gen ,att-r-gen
                               :dmg-r-gen ,dmg-r-gen
                               :def-r-gen ,def-r-gen
                               :ai-cb ,ai-cb)))
       (setf *monster-types-by-level* (remove ,name *monster-types-by-level* :key #'(lambda (x) (name (cadr x))) :test #'string=))
       (push (list ,level ,mt) *monster-types-by-level*)
       (setf *monster-types-by-level* (sort *monster-types-by-level* #'< :key #'car)))))
