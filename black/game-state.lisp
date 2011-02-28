;;;; It is up to the application to keep track of active gamestates
;;;; and set them accordingly using "switch".

(in-package #:black)

(export '(game-state
          add-to-broadcast-receivers
          remove-from-broadcast-receivers
          lookup-by-name
          set-render-order
          every-object))

(defclass game-state ()
  ((state 
    :initform :paused :accessor state 
    :documentation "The current state of the game-state object. Valid
                    options are :playing, :paused. These are
                    controlled through the pause and play generics.")
   (object-manager :initform (make-instance 'object-manager) :reader object-manager)))

(defparameter *game-state* nil)
(declaim ((or game-state null) *game-state*))

(defmethod switch ((obj game-state))
  "Called to switch the active game-state to a different one. It first
   pauses the old game state before switching and then plays the new
   game state."
  (when *game-state*
    (pause *game-state*))
  (setf *game-state* obj)
  (play *game-state*))

(defmethod pause ((obj game-state))
  (setf (state obj) :paused))

(defmethod play ((obj game-state))
  (setf (state obj) :playing))

(defmethod render ((obj game-state))
  (when (eq (state obj) :playing)
    (render (object-manager obj))))

(defmethod update ((obj game-state))
  (when (eq (state obj) :playing)
    (update (object-manager obj))))

(defmethod initialize-instance :after ((obj object) &key)
  "Add newly created object instances to the current game-state."
  (add obj *game-state*))

(defmethod add ((obj object) (state game-state))
  ;; add to the object list
  (add obj (object-manager state)))

(defmethod remove ((obj object) (state game-state))
  (remove obj (object-manager state)))

(defun get-object-by-name (obj-name)
  (declare (simple-string obj-name))
  (let ((obj (gethash obj-name (object-name-lookup (object-manager *game-state*)))))
    (or obj
        (error 'object-nonexistent-error
               :text (format nil "object does not exist: \"~a\"" obj-name)))))

(defun add-to-broadcast-receivers (obj recv-type)
  (with-slots (broadcast-receivers) (object-manager *game-state*)
    (multiple-value-bind (objects hit) (gethash recv-type broadcast-receivers)
      (setf (gethash recv-type broadcast-receivers)
            (if hit
                (pushnew obj objects)          
                (list obj))))))

(defun remove-from-broadcast-receivers (obj recv-type)
  (with-slots (broadcast-receivers) (object-manager *game-state*)
    (multiple-value-bind (objects hit) (gethash recv-type broadcast-receivers)
      (when hit
        (setf (gethash recv-type broadcast-receivers)
              (delete obj objects))

        ;; delete key necessary
        (when (null (gethash recv-type broadcast-receivers))
          (remhash recv-type broadcast-receivers))))))        

(defun lookup-by-name (name)
  (with-slots (object-name-lookup) (object-manager *game-state*)
    (gethash name object-name-lookup)))

;; TODO: test this and remove the render-order stuff from add and remove
(defun set-render-order (order)
  (setf (render-order (object-manager *game-state*))
        order))

(defun every-object (func)
  (loop for val being the hash-value of (object-name-lookup (object-manager *game-state*)) do
       (funcall func val)))