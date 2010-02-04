(in-package #:detome)

(defclass actor ()
  ((x 
    :initarg :x :initform 0 :accessor x :type fixnum :documentation 
    "The x position of the actor on the map.")
   (y 
    :initarg :y :initform 0 :accessor y :type fixnum :documentation 
    "The y position of the actor on the map.")
   (hp
    :initarg :hp :initform 1 :accessor hp :type single-float :documentation
    "The current health of the actor.")))

(defclass player (actor)
  ())
