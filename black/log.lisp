(in-package #:black)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(log)))

(export '(log))

(defmacro log (&rest params)
  #-:logging_disabled
  `(progn (format t ,@params)
	  (terpri))
  #+:logging_disabled
  nil)
