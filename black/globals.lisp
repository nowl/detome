(in-package #:black)

(defvar *screen-width* 320)
(defvar *screen-height* 200)

(defvar *render-order* nil)
(defvar *render-list* nil)

(defvar *object-list* nil)
(defvar *object-name-lookup* (make-hash-table :test #'equal))

(defun reset-globals ()
  (setf *screen-width* 320
		*screen-height* 200
		*render-order* nil
		*render-list* nil
		*object-list* nil
		*object-name-lookup* (make-hash-table :test #'equal)))
