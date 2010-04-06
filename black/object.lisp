(in-package #:black)

(export '(object
          name
          get-object-by-name
          remove-object
          update-cb
          update-cb-control))

(defclass object ()
  ((name
    :initarg :name :accessor name :type simple-string
    :documentation "The textual name of the object used for
                    identification and messaging purposes.")
   (inbox
    :initform nil :accessor inbox :type list
    :documentation "A list of incoming messages to this object.")
   (outbox
    :initform nil :accessor outbox :type list
	:documentation "A list of outgoing messages from this object.")
   (render-cb
	:initform nil :initarg :render-cb :accessor render-cb :type function
	:documentation "A render callback called to render the object. The
                    callback gets passed the current object and the
                    interpolation as the arguments.")
   (update-cb
	:initform nil :initarg :update-cb :accessor update-cb :type function
	:documentation "An update callback called to update the
                    object. The callback gets passed the current
                    object as the argument.")
   (update-cb-control 
    :initform :all :initarg :update-cb-control :accessor update-cb-control :type (or keyword cons)
    :documentation "This slot controls how often the update-cb is
                    called. Possibly values include :all for the
                    callback to be called every update, (:time
                    second-value) for the callback to be called every
                    second-value seconds, (:tick ticks) for the
                    callback to be called every ticks ticks, :one-shot
                    to be called only once, :none to never be
                    called, (:turns N) to be called when the cadr is
                    greater than 0, N will be decremented if update is
                    called.")))

(define-condition object-nonexistent-error (error) 
  ((text :initarg :text :reader text)))

(defmethod print-object ((obj object-nonexistent-error) stream)
  (write-string (text obj) stream))

(defun get-object-by-name (obj-name)
  (declare (simple-string obj-name))
  (let ((obj (gethash obj-name *object-name-lookup*)))
    (or obj 
        (error 'object-nonexistent-error
               :text (format nil "object does not exist: \"~a\"" obj-name)))))

(defmethod initialize-instance :after ((obj object) &key)
  "Add newly created object instances to the global object lists."

  (remove-object (name obj))
  
  ;; add to the name hashtable
  (with-slots (name) obj
    (multiple-value-bind (obj-in-hash hit) (gethash name *object-name-lookup*)
      (declare (ignore obj-in-hash))
      (if hit
          (warn "Initializing an object of the same name \"~a\"" name)
          (setf (gethash name *object-name-lookup*) obj))))
  
  ;; add to the global list
  (pushnew obj *object-list*))

(defun remove-object (object)
  "Remove object by name from the object lists."
  (declare (simple-string object))
  (setf *object-list* (delete object *object-list* :test #'equal :key #'name))
  (remhash object *object-name-lookup*))