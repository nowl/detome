(in-package #:black)

(export '(define-object))

(defmacro define-object (&rest args)
  `(make-instance 'black:object
				  ,@args))