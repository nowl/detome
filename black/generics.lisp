(in-package #:black)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(remove)))

(defgeneric add (src dest)
  (:documentation
   "This represents the concept of adding a source object to a
   destination object."))

(defgeneric remove (src dest)
  (:documentation
   "This represents the concept of removing a source object from a
   destination object."))

(defgeneric switch (obj)
  (:documentation
   "This represents the concept of switching the system to a different
   object."))

(defgeneric pause (obj)
  (:documentation
   "This represents the act of 'pausing' a specific object."))

(defgeneric play (obj)
  (:documentation
   "This represents the act of 'playing' a specific object."))

(defgeneric render (obj)
  (:documentation
   "This represents the act of 'render' a specific object."))

(defgeneric update (obj)
  (:documentation
   "This represents the act of 'updating' a specific object."))
