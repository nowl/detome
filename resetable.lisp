(in-package #:detome)

(defmacro def-resetable-var (var value &optional doc &key (reset-var '*global-resets*))
  `(progn 
     ,(if doc
          `(defvar ,var ,value ,doc)
          `(defvar ,var ,value))
     (push #'(lambda () (setf ,var ,value)) ,reset-var)))

(defvar *global-resets* nil)

(defun reset-globals ()
  (dolist (reset-func (reverse *global-resets*))
    (funcall reset-func)))

