(in-package #:black)

(defclass object-manager ()
  ((objects :initform nil :accessor objects :type list)
   (object-name-lookup :initform (make-hash-table :test #'equal)
                       :reader object-name-lookup :type hash-table)
   (broadcast-receivers :initform (make-hash-table :test #'equal)
                        :reader broadcast-receivers :type hash-table)))   

