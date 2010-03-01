(in-package #:detome)

(defclass actor (object)
  ((meta
    :initarg :meta :initform nil :accessor meta :type list :documentation 
    "An alist containing metadata for use with supporting functions.")
   (x 
    :initarg :x :accessor x :type fixnum :documentation 
    "The x position of the actor on the map.")
   (y 
    :initarg :y :accessor y :type fixnum :documentation 
    "The y position of the actor on the map.")
   (level
    :initarg :level :accessor level :type fixnum :documentation 
    "The actor's level.")
   (hp
    :initarg :hp :accessor hp :type single-float :documentation
    "The current health of the actor.")
   (att-r
	:initarg :att-r :accessor att-r :type single-float :documentation
	"Attack rating of the actor.")
   (dmg-r
	:initarg :dmg-r :accessor dmg-r :type single-float :documentation
	"Damage rating of the actor.")
   (def-r
	:initarg :def-r :accessor def-r :type single-float :documentation
	"Defence rating of the actor.")
   (inv
	:initarg :inv :accessor inv :type list :documentation
	"Actor's inventory.")))

(defclass player (actor)
  ()
  (:default-initargs :inv nil :level 1))
