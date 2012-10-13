(in-package :detome)

(defmacro gh (ht key)
  `(gethash ,key ,ht))

(defmacro sh (ht &rest keys-and-values)
  (flet ((make-pairs ()
           (loop for a in keys-and-values by #'cddr 
              for b in (cdr keys-and-values) by #'cddr collecting
                (list a b))))
    `(setf ,@(loop for pair in (make-pairs) appending
                  `((gethash ,(car pair) ,ht)
                    ,(cadr pair))))))
