(in-package #:blackfish)

(defparameter *broadcast-receivers* (make-hash-table))

(defstruct message
  (sender nil :type (or keyword null))
  (receiver nil :type (or keyword null))
  (type :generic :type keyword)
  (payload))

;;; A receiving object must implement the following method, this is
;;; called to deliver a message to the object.
(defgeneric process-message (obj message delivery-type))

;;; External call to add a receiver to the broadcast messages for the
;;; specific type.
(defun add-message-listener (receiver type)
  (declare (symbol type))
  (multiple-value-bind (val exists) (gethash type *broadcast-receivers*)
    (if exists
        (pushnew receiver val)
        (setf (gethash type *broadcast-receivers*)
              (list receiver)))))

;;; External call to remove a receiver to the broadcast messages for
;;; the specific type.
(defun remove-message-listener (receiver type)
  (declare (symbol type))
  (multiple-value-bind (val exists) (gethash type *broadcast-receivers*)
    (when exists
      (setf (gethash type *broadcast-receivers*)
            (delete receiver val)))))


;; Send the message to all valid message listeners.
(defun pass-to-broadcast-receivers (message delivery-type)  
  (declare (message message)
           (symbol delivery-type))
  (some #'identity
        (loop for rec in (gethash (message-type message) *broadcast-receivers*) collect
             (process-message rec message delivery-type))))
  
;;; Send a message into the message bus.
(defun send-message (&key sender receiver (type :generic) payload (delivery-type :sync))
  (let ((mes (make-message :sender sender
                           :receiver receiver
                           :type type
                           :payload payload)))
    (if receiver
        (process-message receiver mes delivery-type)
        (pass-to-broadcast-receivers mes delivery-type))))