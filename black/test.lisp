(black:set-log-level :debug)

(defparameter *game-state* (make-instance 'black:game-state))

(black:switch *game-state*)

(make-instance 'black:object 
               :name "first object"
               :update-cb #'(lambda (obj)
                              (format t "calling update"))
               :update-cb-control '(:seconds 5))

(make-instance 'black:object 
               :name "second object"
               :update-cb #'(lambda (obj)
                              (format t "calling update"))
               :update-cb-control '(:seconds 5))

(black:add-to-broadcast-receivers (black:lookup-by-name "first object") "sdl-event")
(black:add-to-broadcast-receivers (black:lookup-by-name "second object") "sdl-event")

(setf (gethash :sdl-event-cb (black:meta (black:lookup-by-name "first object")))
      #'(lambda (event-type &rest args)
          (case event-type
            (:quit-event
             (black:log :info "received quit event")
             t)
            (:key-down-event
             (destructuring-bind
                   (_1 state _2 scancode _3 key _4 mod _5 mod-key _6 unicode) args
               (when (eq key :sdl-key-escape)
                 (black:log :info "got escape key")))))))

(setf (gethash :sdl-event-cb (black:meta (black:lookup-by-name "second object")))
      #'(lambda (event-type &rest args)
          (case event-type
            (:key-down-event
             (destructuring-bind
                   (_1 state _2 scancode _3 key _4 mod _5 mod-key _6 unicode) args
               (when (eq key :sdl-key-a)
                 (black:log :info "got 'a' key")))))))

           

;;(black:remove-from-broadcast-receivers (black:lookup-by-name "first object") "sdl-event")
;;(black:remove (black:lookup-by-name "first object") *game-state*)

