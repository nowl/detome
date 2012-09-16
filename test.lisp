(in-package :blacker)

(make-component
 "sdl receiver"
 '(:sdl-event)
 #'(lambda (message receiver)
     (declare (ignore receiver))
     (format t "received internal :sdl-event ~a~%" (payload message))
     (cond
       ((eql (car (payload message)) :quit-event) t)
       ((and (member :key-down-event (payload message))
             (eql (second (member :key (payload message)))
                  :sdl-key-escape))
        (push-quit-event)))))

(defparameter *system-listener-entity*
  (make-entity '("sdl receiver")))

(make-component
 "updatable"
 '(:system-update)
 #'(lambda (message receiver)
     (format t "update is being called on the ~a object~%" receiver)))

(defparameter *foo*
  (make-entity '("updatable")))

;(mainloop 'test)