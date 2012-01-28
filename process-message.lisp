(in-package :detome)

(bf:add-message-listener 'detome :sdl-event)

(defgeneric process-message (sender receiver type delivery-type payload))

(defmethod process-message (sender receiver (type (eql :sdl-event)) delivery-type payload)
  (bf:log :info ":sdl-event ~a~%" payload)
  (case (car payload)
    (:key-down-event (let ((key (cadr (member :key payload))))
                       (cond
                         ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
                         ((sdl:key= key :sdl-key-e) (test-for-action-at-location) t)
                         #.(gen-move-command :sdl-key-kp4 -1 0)
                         #.(gen-move-command :sdl-key-kp7 -1 -1)
                         #.(gen-move-command :sdl-key-kp8 0 -1)
                         #.(gen-move-command :sdl-key-kp9 1 -1)
                         #.(gen-move-command :sdl-key-kp6 1 0)
                         #.(gen-move-command :sdl-key-kp3 1 1)
                         #.(gen-move-command :sdl-key-kp2 0 1)
                         #.(gen-move-command :sdl-key-kp1 -1 1))))
    (:quit-event t)))

(defmethod bf:process-message ((obj (eql 'detome)) message delivery-type)
  (process-message (bf:message-sender message)
                   (bf:message-receiver message)
                   (bf:message-type message)
                   delivery-type
                   (bf:message-payload message)))
