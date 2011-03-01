(in-package #:detome)

(defclass item-type ()
  ((name
    :initarg :name :accessor name :type simple-string
    :documentation
    "A descriptive name for this object type.")
   (image-name
    :initarg :image-name :accessor image-name :type simple-string
    :documentation 
    "The image name of this monster type")
   (cls
	:initarg :cls :accessor cls :type keyword
    :documentation
	"The class of this item-type. :weapon, :armor, etc. This
    essentially determines how the item can be used.")
   (cb
	:initarg :cb :accessor cb :type function
    :documentation
	"This is a function whose arguments are determined by the
	item-type's class and is called whenever the item is 'used'.")
   (level
	:initarg :level :accessor level :type fixnum
    :documentation
	"The item-type's level meaning the minimum required level to use
    the item and also affects which monsters may spawn this
    item-type.")))

(defclass item (map-object)
  ((location 
	:initarg :location :accessor location :type list
    :documentation
	"The first element of this list is the parent object, actor,
    etc. to which this item belongs. The rest of the elements in the
    list are parent specific. For example if the first element is a
    map object then the second and third may be the grid coordinates
    on the map where this item is located, if the parent is an actor
    the arguments may be where on the player the object is located
    including their inventory.")
   (item-type
	:initarg :item-type :accessor item-type :type item-type
    :documentation
	"The type of this item."))
  (:default-initargs :x 0 :y 0 :name "item"))

;; tie getting and setting x and y to the location slot

(defmethod x ((item item))
  (case (car (location item))
    (:map (second (location item)))))

(defmethod y ((item item))
  (case (car (location item))
    (:map (third (location item)))))

(defmethod (setf x) ((item item) val)
  (if (and (consp (location item))
           (eq :map (first (location item))))
      (setf (location item) `(:map ,val ,(third (location item))))
    (setf (location item) `(:map ,val 0))))

(defmethod (setf y) ((item item) val)
  (if (and (consp (location item))
           (eq :map (first (location item))))
      (setf (location item) `(:map ,(second (location item)) ,val))
    (setf (location item) `(:map 0 ,val))))
	
(defparameter *item-types* (make-hash-table :test #'equal))
(defparameter *items-in-level* nil)

(defmacro make-item-type (name image-name type callback level)
  `(setf (gethash ,name *item-types*)
         (make-instance 'item-type
                        :name ,name
                        :image-name ,image-name
                        :cls ,type
                        :cb ,callback
                        :level ,level)))

(defmacro get-item-type (name)
  `(gethash ,name *item-types*))
