(in-package #:black)

(export '(object
          name
          get-object-by-name
          remove-object
          update-cb
          update-cb-control))

(defclass object ()
  ((name
    :initarg :name :accessor name :type simple-string
    :documentation "The textual name of the object used for
                    identification and messaging purposes.")
   (inbox
    :initform nil :accessor inbox :type list
    :documentation "A list of incoming messages to this object.")
   (render-level
    :initform "base" :initarg :render-level :accessor render-level
	:documentation "The layer to render this object on.")
   (render-cb
	:initform nil :initarg :render-cb :accessor render-cb :type function
	:documentation "A render callback called to render the object. The
                    callback gets passed the current object and the
                    interpolation as the arguments.")
   (update-cb
	:initform nil :initarg :update-cb :accessor update-cb :type function
	:documentation "An update callback called to update the
                    object. The callback gets passed the current
                    object as the argument.")
   (update-cb-control 
    :initform :all :initarg :update-cb-control :accessor update-cb-control :type (or keyword cons)
    :documentation "This slot controls how often the update-cb is
                    called. Possibly values include :all for the
                    callback to be called every update, (:time
                    second-value) for the callback to be called every
                    second-value seconds, (:tick ticks) for the
                    callback to be called every ticks ticks, :one-shot
                    to be called only once, :none to never be
                    called, (:turns N) to be called when the cadr is
                    greater than 0, N will be decremented if update is
                    called.")
   (meta
    :initform (make-hash-table) :reader meta :type hash-table
    :documentation "This can serve as any extra information to be
                    stored in the object. Some uses include callbacks
                    back into the object to handle input events.")))

(defmethod print-object ((obj object) stream)
  "Print the game object by displaying it's name"
  (write-string (name obj) stream))

(define-condition object-nonexistent-error (error) 
  ((text :initarg :text :reader text)))

(defmethod print-object ((obj object-nonexistent-error) stream)
  (write-string (text obj) stream))

(defmethod update ((obj object))
  (with-slots (update-cb update-cb-control) obj
    (flet ((call-update ()
             (when update-cb
               (funcall update-cb obj))))
      (etypecase update-cb-control
        (keyword (ecase update-cb-control
                   (:all (call-update))
                   (:one-shot (call-update)
                              (setf update-cb-control :none))
                   (:none nil)))
        (cons (ecase (first update-cb-control)
                (:ticks (let ((ticks (second update-cb-control)))
                          (when (eql (mod *game-tick* ticks) 0)
                            (call-update))))
                (:turns (let ((num-turns (second update-cb-control)))
                          (when (> num-turns 0)
                            (call-update)
                            (setf (second update-cb-control) (decf num-turns)))))
                (:seconds (let* ((seconds (second update-cb-control))
                                 (modulo (ceiling (/ (* seconds 1000) *ms-per-update*))))
                            (when (eql (mod *game-tick* modulo) 0)
                              (call-update))))))))))

(defmethod render ((obj object))
  (let ((render-cb (render-cb obj)))
    (and render-cb (funcall render-cb obj *interpolation*))))