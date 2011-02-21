(in-package #:black)

(export '(make-object
          set-meta))

(defmacro make-object (&rest args)
  `(make-instance 'black:object
				  ,@args))

;; convenient macro for setting a specific metadata value in the
;; object
(defmacro set-meta ((type object) &body body)
  (let ((foo (gensym)))
    `(let ((,foo ,object))
       (setf (gethash ,type (black:meta
                             (etypecase ,foo
                               (string (black:lookup-by-name ,foo))
                               (object ,foo))))
             ,@body))))
  