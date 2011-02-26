(in-package #:black)

(export '(make-object
          set-meta
          get-meta
          with-gensyms))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-vars (vars)
    (loop for var in vars collect
         `(,var (gensym)))))

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(collect-vars vars)
     ,@body))

(defmacro make-object (&rest args)
  `(make-instance 'black:object
                  ,@args))

;; convenient macro for setting a specific metadata value in the
;; object
(defmacro set-meta ((type object) &body body)
  `(setf (gethash ,type (black:meta
                         (etypecase ,object
                           (string (black:lookup-by-name ,object))
                           (object ,object))))
         ,@body))
  
(defmacro get-meta (type object)
  `(gethash ,type (black:meta
                   (etypecase ,object
                     (string (black:lookup-by-name ,object))
                     (object ,object)))))
