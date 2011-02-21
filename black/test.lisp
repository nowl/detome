(defparameter *game-state* (make-instance 'black:game-state))

(black:switch *game-state*)

(make-instance 'black:object 
               :name "first object"
               :update-cb #'(lambda (obj)
                              (format t "calling update")))

(black:add-to-broadcast-receivers (black:lookup-by-name "first object") "sdl-event")

