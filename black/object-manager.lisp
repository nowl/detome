(in-package #:black)

(defclass object-manager ()
  ((objects :initform nil :accessor objects :type list)
   (broadcast-receivers :initform nil :accessor broadcast-receivers :type hash-table)))

(defparameter *object-manager* (make-instance 'object-manager))
