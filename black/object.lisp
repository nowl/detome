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
   (render-level
    :initform 0 :initarg :render-level :accessor render-level :type fixnum
	:documentation "The layer to render this object on.")
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

(defmethod print-object ((obj object) stream)
  "Print the game object by displaying it's name"
  (write-string (name obj) stream))

(define-condition object-nonexistent-error (error) 
  ((text :initarg :text :reader text)))

(defmethod print-object ((obj object-nonexistent-error) stream)
  (write-string (text obj) stream))

(defun get-object-by-name (obj-name)
  (declare (simple-string obj-name))
  (let ((obj (gethash obj-name (object-name-lookup (object-manager *game-state*)))))
    (or obj
        (error 'object-nonexistent-error
               :text (format nil "object does not exist: \"~a\"" obj-name)))))

(defmethod initialize-instance :after ((obj object) &key)
  "Add newly created object instances to the current game-state."
  (add obj *game-state*))

(defmethod add ((obj object) (state game-state))
  ;; add to the object list
  (add obj (object-manager state)))

(defmethod add ((obj object) (manager object-manager))
  ;; add to the name hashtable
  (with-slots (object-name-lookup objects) manager
    (with-slots (name) obj
      (multiple-value-bind (obj-in-hash hit) (gethash name object-name-lookup)
        (declare (ignore obj-in-hash))
        (if hit
            (warn "Initializing an object of the same name \"~a\"" name)
            (setf (gethash name object-name-lookup) obj))))

    ;; TODO: perhaps test for duplicate or an existing object of the
    ;; same name before adding?
    (pushnew obj objects)))

(defmethod remove ((obj object) (state game-state))
  (remove obj (object-manager state)))

(defmethod remove ((obj object) (manager object-manager))
  "Remove object from the object lists and name hash."
  (with-slots (objects object-name-lookup) manager
    (setf objects
          (delete obj objects :test #'equal :key #'name))
    (remhash obj object-name-lookup)))
