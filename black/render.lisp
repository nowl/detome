(in-package #:black)

(export '(add-to-render-list
	  clear-render-list
          remove-from-render-list))

(defun render-obj (obj interpolation)
  (declare (object obj)
           (single-float interpolation))
  (let ((render-cb (render-cb obj)))
    (and render-cb (funcall render-cb obj interpolation))))

(defun add-to-render-list (obj)
  (declare ((or object simple-string) obj))
  (pushnew (etypecase obj
             (simple-string (get-object-by-name obj))
             (object obj))
           *render-list*))

(defun remove-from-render-list (obj)
  (declare ((or object simple-string) obj))
  (setf *render-list*
	(delete (etypecase obj
		  (simple-string (get-object-by-name obj))
		  (object obj))
		*render-list*)))

(defun clear-render-list ()
  (setf *render-list* nil))

#|
(defun add-to-render-list (type renderer)
  (declare (symbol type) (renderer renderer))
  (let ((ass (assoc type *render-list*)))
        (if ass
                (push renderer (cadr ass))
                (push (list type (list renderer)) *render-list*))))

(defmethod print-object ((object renderer) out)
  (format out "<renderer:\"~a\">" (slot-value object 'name)))
           

(defclass renderer-sub (renderer)
  ((blah :initarg :blah :accessor blah))
  (:default-initargs :name "some subclass of renderer"))
|#