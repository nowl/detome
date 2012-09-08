(in-package #:blacker)

(defparameter *log-levels*
  '(:error
	:warning
	:info
	:debug))

(defparameter *log-types-to-print* *log-levels*)

(defun set-log-level (level)
  (setf *log-types-to-print*
		(loop for lev in *log-levels* collect lev until (eq lev level))))

(defmacro log (level &rest params)
  #-:logging_disabled
  `(when (member ,level *log-types-to-print*)
	 (format t ,@params)
	 (terpri))
  #+:logging_disabled
  nil)
