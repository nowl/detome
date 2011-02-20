(in-package #:black)

(defclass object-manager ()
  ((object-layers
    :initform (make-hash-table :test #'equal) :reader object-layers :type hash-table
    :documentation "This table maps a given render level to a sequence
                    of objects.")
   (render-order
    :initform nil :accessor render-order :type list
    :documentation "This represents the order objects are
                    rendered. This is a list of keys into the objects
                    hash.")
   (object-name-lookup
    :initform (make-hash-table :test #'equal) :reader object-name-lookup :type hash-table)
   (broadcast-receivers
    :initform (make-hash-table :test #'equal) :reader broadcast-receivers :type hash-table)))   

(defmethod render ((obj object-manager))
  (with-slots (render-order object-layers) obj
              (loop for layer in render-order do
                    (let ((objects (gethash layer object-layers))) 
                      (loop for o in objects do
                            (render o))))))

(defmethod update ((obj object-manager))
  (loop for objects being the hash-values of (object-layers (object-manager *game-state*)) do
       (loop for obj in objects do
            (update obj))))

(defmethod add ((obj object) (manager object-manager))
  ;; add to the name hashtable
  (with-slots (object-name-lookup object-layers render-order) manager
    (with-slots (name) obj
      (multiple-value-bind (obj-in-hash hit) (gethash name object-name-lookup)
        (declare (ignore obj-in-hash))
        (if hit
            (warn "Initializing an object of the same name \"~a\"" name)
            (setf (gethash name object-name-lookup) obj))))

    ;; add level to the render-order if it doesn't exist
    (pushnew (render-level obj) render-order :test #'equal)
    
    ;; TODO: perhaps test for duplicate or an existing object of the
    ;; same name before adding?
    ;; add to the object-layers
    (multiple-value-bind (objects hit) (gethash (render-level obj) object-layers)
      (if hit
          (pushnew obj objects)
          (setf (gethash (render-level obj) object-layers)
                (list obj))))))

(defmethod remove ((obj object) (manager object-manager))
  "Remove object from the object lists, the name hash, and the
   render-layers if necessary."
  (with-slots (object-name-lookup object-layers render-order) manager
    (multiple-value-bind (objects hit) (gethash (render-level obj) object-layers)
      (when hit
        (setf objects
              (delete obj objects))
        (when (null objects)
          (remhash (render-level obj) object-layers)
          (setf render-order
                (delete obj render-order :test #'equal :key #'render-level)))))
    (remhash (name obj) object-name-lookup)))
