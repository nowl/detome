(in-package #:black)

(export '(game-state
          switch-game-state))

(defparameter *game-state* nil)

(defclass game-state ()
  ((object-manager :initform (make-instance 'object-manager) :reader object-manager)))

(defmethod switch ((obj game-state))
  (setf *game-state* obj))
