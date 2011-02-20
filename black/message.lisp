(in-package #:black)

(export '(make-and-send-message))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(type)))

(defclass message ()
  ((sender
    :initarg :sender :accessor sender :type (or simple-string object)
    :documentation "The sender of this message.")
   (receiver
    :initarg :receiver :initform nil :accessor receiver :type (or simple-string object)
    :documentation "The receiver of this message. If NIL then a
                    broadcast message is implied")
   (type
    :initarg :type :initform nil :accessor type :type (or simple-string symbol)
    :documentation "The type of the message represents metadata about
                    what the origin of the message is. For example an
                    sdl-event message for use as a broadcast
                    message.")
   (action
    :initarg :action :accessor action :type function
    :documentation "Action is a function which takes the sender object
                    and the recipient object of the message as an
                    argument")))

(defun process-message (message)
  (declare (message message))
  (funcall (action message) (sender message) (receiver message) (type message)))

(defun deliver-broadcast-message (message type)
  (declare (message message))
  (if (null (type message))
      (error 'object-nonexistent-error :text (format nil "broadcast message has no type")))
  (unless *game-state*
    (return-from deliver-broadcast-message))
  (multiple-value-bind (objs hit) 
      (gethash (type message) (broadcast-receivers (object-manager *game-state*)))
    (when hit
      (ecase type
        (:sync
         (loop for obj in objs do
              (push message (inbox obj))))
        (:async
         (loop for obj in objs do
              (setf (receiver message) obj)
              (when (process-message message)
                (return))))))))

(defun deliver-directed-message (message type)
  (declare (message message))
  (ecase type
    (:sync (multiple-value-bind (obj hit) (gethash (receiver message) *object-name-lookup*)
             (if hit
                 (push message (inbox obj))
               (error 'object-nonexistent-error 
                      :text (format nil "object does not exist: \"~a\"" (receiver message))))))
    (:async (process-message message))))

(defun process-messages-for-obj (obj)
  "Processes all messages in a given object's inbox in the order they
  were sent."
  (declare (object obj))
  (let ((messages (reverse (inbox obj))))                         
	(loop for message in messages do
          (process-message message))
	(setf (inbox obj) nil)))

(defun process-messages ()
  "Processes all messages in each object's inbox in the order they
  were sent."
  (when *game-state*
    (loop for objects being the hash-values of (object-layers (object-manager *game-state*)) do
         (loop for obj in objects do
              (process-messages-for-obj obj)))))

(defun deliver-message (message &optional (type :sync))
  (declare (message message)
           (symbol type))
  (if (null (receiver message))
      (deliver-broadcast-message message type)
    (deliver-directed-message message type)))

(defmacro make-and-send-message (&key sender receiver (mes-type nil) action (type :sync))
  `(deliver-message (make-instance 'message 
                                   :sender ,sender 
                                   :receiver ,receiver 
                                   :action ,action 
                                   :type ,mes-type) ,type))
