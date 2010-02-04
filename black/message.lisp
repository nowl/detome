(in-package #:black)

(export '(make-and-send-message))

(defstruct message
  (sender nil :type (or simple-string object))
  (receiver nil :type (or simple-string object))
  ;; action is a function which takes the sender object and the
  ;; recipient object of the message as an argument
  (action #'(lambda (n) (declare (ignore n)) nil) :type function))

(defun deliver-message (message)
  (declare (message message))
  (multiple-value-bind (obj hit) (gethash (message-receiver message) *object-name-lookup*)
    (if hit
        (push message (inbox obj))
        (error 'object-nonexistent-error :text (format nil "object does not exist: \"~a\"" (message-receiver message))))))

(defun skip-object-restart (c)
  (invoke-restart (find-restart 'skip-message) c))

(defun route-messages ()
  "Delivers all outgoing messages to the recipients."
  (handler-bind ((object-nonexistent-error #'skip-object-restart))
    (loop for obj in *object-list* do
         (loop for message in (outbox obj) do
              (restart-case (deliver-message message)
                (skip-message (c) (format t "message can not be delivered:~%>> ~a~%" c))))
         (setf (outbox obj) nil))))

(defun process-message (message receiver)
  (declare (message message)
           (object receiver))
  (let ((sender (etypecase (message-sender message)
                  (simple-string (multiple-value-bind (obj hit) (gethash (message-sender message) *object-name-lookup*)
                                   (if hit
                                       obj
                                       (error 'object-nonexistent-error
                                              :text (format nil "object does not exist: \"~a\"" (message-sender message))))))
                  (object (message-sender message)))))
    (funcall (message-action message) sender receiver)))

(defun process-messages-for-obj (obj)
  "Processes all messages in a given object's inbox in the order they
  were sent."
  (let ((messages (reverse (inbox obj))))                         
	(loop for message in messages do
		 (restart-case (process-message message obj)
		   (skip-message (c) (format t "message can not be processed:~%>> ~a~%" c))))
	(setf (inbox obj) nil)))

(defun process-messages ()
  "Processes all messages in each object's inbox in the order they
  were sent."
  (handler-bind ((object-nonexistent-error #'skip-object-restart))
    (loop for obj in *object-list* do
		 (process-messages-for-obj obj))))

(defun send-message (message)
  "This should be called to put a message in the sender's outbox for
  delivery on the next update."
  (declare (message message))
  (let ((sender (message-sender message)))
    (etypecase sender
      (simple-string (multiple-value-bind (obj hit) (gethash sender *object-name-lookup*)
                       (if hit
                           (push message (outbox obj))
                           (error "Can not put message in outbox for object name \"~a\" (doesn't exist)" sender))))
      (object (push message (outbox sender))))))

(defmacro make-and-send-message (&key sender receiver action)
  `(send-message (make-message :sender ,sender :receiver ,receiver :action ,action)))