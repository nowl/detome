(in-package #:blacker)

;;; definitions of entities, components, messages, ...

(defclass entity ()
  ((meta
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
   (entities
    :initform nil :accessor entities :type list
    :documentation
    "A list of the entities containing this component.")))

(defclass message ()
  ((sender
    :initarg :sender :accessor sender :type entity
    :documentation
    "The specific entity type that has sent this message.")
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

(defparameter *name-entity* (make-hash-table :test #'equal)
  "Reverse name lookup for entities.")

(defparameter *name-component* (make-hash-table :test #'equal)
  "Reverse name lookup for components.")

(defparameter *responder-type-component-list*
  (make-hash-table)
  "Lookup components that can respond to a specific response type.")

(defparameter *messages* nil
  "A list of all messages waiting to be processed.")

;;; public engine functions

;; looks up an entity by name
(defun name-entity (name)
  (gethash name *name-entity*))

;; name: a string naming the entity
;; components: a list of component names to add to the entity
(defun make-entity (name &optional components)
  (declare (list components))
  (multiple-value-bind (* exist) (gethash name *name-entity*)
    (when exist (error "trying to create a duplicate entity ~a" name)))
  (let ((entity (make-instance 'entity)))
    
    ;; add entity to each component
    (loop for comp in components do
         (multiple-value-bind (comp-obj exist) (gethash comp *name-component*)
           (unless exist (error "trying to add a component that does not exist ~a" comp))
           (push entity (entities comp-obj))))

    ;; add entity to globals
    (setf (gethash name *name-entity*) entity)))
       
;; name: a string naming the component
;; message-types: a list of symbols of the message types this
;;   component will respond to
;; responder: a function taking a message object and an owner entity
;;   that is called during a message broadcast
(defun make-component (name message-types responder)
  (declare (list message-types)
           (function responder))
  (multiple-value-bind (* exist) (gethash name *name-component*)
    (when exist (error "trying to create a duplicate component ~a" name)))
  (let ((component (make-instance 'component
                                  :responder responder)))
    ;; add to globals
    (setf (gethash name *name-component*) component)
    (loop for message-type in message-types do
         (multiple-value-bind (components exist) 
             (gethash message-type *responder-type-component-list*)
           (setf (gethash message-type *responder-type-component-list*)
                 (if exist
                     (cons component components)
                     (list component)))))))

(defun send-message (type payload sender &optional (delivery-type :sync))
  (declare (symbol type delivery-type))
  (let ((message
         (make-instance 'message :payload payload :type type :sender sender)))
    (ecase delivery-type
      (:async (process-message message))
      (:sync (push message *messages*)))))

;;; internal functions

(defun process-message (message)
  (loop for component in (gethash (type message) *responder-type-component-list*) do
       (loop for entity in (entities component) do
            (when (funcall (responder component) message entity)
              (return-from process-message t)))))

;; message processing loop
(defun process-messages ()
  (loop for message in *messages* do (process-message message))
  (setf *messages* nil))
                
