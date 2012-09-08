(in-package #:blacker)

;;; definitions of entities, components, messages, ...

(defclass entity ()
  ((components 
    :initarg :components :initform nil :accessor components :type list
    :documentation
    "List of all components contained within this entity.")
   (meta
    :initform (make-hash-table) :accessor meta :type hash-table
    :documentation
    "Arbitrary metadata stored in this entity for use by the
    components.")))

(defclass component ()
  ((responder
    :initarg :responder :initform nil :accessor responder :type function
    :documentation
    "This is a function which takes a message as an argument. It is
    called on the component when one of the valid message types is
    broadcast.")
   (responder-messages
    :initarg :responder-messages :initform nil :accessor responder-messages :type list
    :documentation
    "A list of the valid message response types accepted by this
    component.")))

(defclass message ()
  ((sender
    :initarg :sender :accessor sender :type entity
    :documentation
    "The specific entity type that has sent this message.")
   (owner
    :initarg :owner :accessor owner :type entity
    :documentation
    "The owner of the respective component receiving this message.")
   (type
    :initarg :type :accessor type :type symbol
    :documentation
    "The type of this message, checked against the accepted types for
    each component.")
   (payload
    :initarg :payload :initform nil :accessor payload
    :documentation
    "Arbitrary info holder for the message payload. Contents will
    depend on the type.")))

;;; global engine variables

(defparameter *entities* nil
  "The global list of entities. This is used while scanning entities
  for delivery of messages.")

(defparameter *name-entity* (make-hash-table :test #'equal)
  "Reverse name lookup for entities.")

(defparameter *name-component* (make-hash-table :test #'equal)
  "Reverse name lookup for components.")

;;; public engine functions

;; name: a string naming the entity
;; components: a list of component names to add to the entity
(defun make-entity (name &optional components)
  (declare (list components))
  (multiple-value-bind (* exist) (gethash name *name-entity*)
    (when exist (error "trying to create an entity of the same name ~a" name)))
  (let ((entity (make-instance 'entity)))
    (push entity *entities*)
    (setf (gethash name *name-entity*) entity)
    
    ;; add components to entity
    (loop for comp in components do
         (multiple-value-bind (comp-obj exist) (gethash comp *name-component*)
           (unless exist (error "trying to add a component that does not exist ~a" comp))
           (push comp-obj (components entity))))))
         
       
;(defun make-component (name 
