(in-package :blacker)

(export '(meta
          responder
          sender
          type
          payload
          make-component))

;;; definitions of components, messages, ...

(defclass component ()
  ((responder
    :initarg :responder :initform nil :accessor responder :type function
    :documentation
    "This is a function which takes a message as an argument. It is
    called on the component when one of the valid message types is
    broadcast.")))

(defclass message ()
  ((type
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

(defparameter *name-component* (make-hash-table :test #'equal)
  "Reverse name lookup for components.")

(defparameter *responder-type-component-list*
  (make-hash-table)
  "Lookup components that can respond to a specific response type.")

(defparameter *messages* nil
  "A list of all messages waiting to be processed.")

;;; public engine functions

(defun add-component-to-responder-types (component message-types)
  (loop for message-type in message-types do
       (multiple-value-bind (components exist) 
           (gethash message-type *responder-type-component-list*)
         (setf (gethash message-type *responder-type-component-list*)
               (if exist
                   (cons component components)
                   (list component))))))

(defun remove-component-from-responder-types (component)
  (loop for message symbol being the hash-keys of *responder-type-component-list* using (hash-value components) do
       (when (member component components)
         (setf (gethash message *responder-type-component-list*)
               (delete component components)))))

;; name: a string naming the component
;; message-types: a list of symbols of the message types this
;;   component will respond to
;; responder: a function taking a message object that is called during
;;   a message broadcast
(defun make-component (name message-types responder)
  (declare (list message-types)
           (function responder))
    (multiple-value-bind (existing-comp exist) (gethash name *name-component*)
      (if exist
          (progn
            ;;(warn "trying to create a duplicate component ~a, modifying existing" name)
            (setf (responder existing-comp) responder)
            (remove-component-from-responder-types existing-comp)
            (add-component-to-responder-types existing-comp message-types)
            existing-comp)
          (let ((new-comp (make-instance 'component :responder responder)))
            (setf (gethash name *name-component*) new-comp)
            (add-component-to-responder-types new-comp message-types)
            new-comp))))

(defun send-message (type payload &optional (delivery-type :sync))
  (declare (symbol type delivery-type))
  (let ((message
         (make-instance 'message :payload payload :type type)))
    (ecase delivery-type
      (:async (process-message message))
      (:sync (push message *messages*)))))

;;; internal functions

(defun process-message (message)
  (loop for component in (gethash (type message) *responder-type-component-list*) do
       (when (funcall (responder component) message)
         (return-from process-message t))))

;; message processing loop
(defun process-messages ()
  (loop for message in *messages* do (process-message message))
  (setf *messages* nil))
