(in-package #:black)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(log)))

(export '(log set-log-level))

(defvar *log-types-to-print* nil)

(defvar *log-levels*
  '(:error
	:warning
	:info
	:debug))

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
