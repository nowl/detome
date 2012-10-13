(in-package :detome)

;; helper macros for shorthand ways of grabbing meta info from
;; hashtables

(defmacro gh (ht key)
  `(gethash ,key ,ht))

(defmacro with-ghs ((&rest keys) ht &body body)
  `(let ,(loop for key in keys collect
              `(,key (gh ,ht ',key)))
     ,@body))

(defmacro sh (ht &rest keys-and-values)
  (flet ((make-pairs ()
           (loop for a in keys-and-values by #'cddr 
              for b in (cdr keys-and-values) by #'cddr collecting
                (list a b))))
    `(setf ,@(loop for pair in (make-pairs) appending
                  `((gethash ,(car pair) ,ht)
                    ,(cadr pair))))))

(defun draw-text (text x y r g b)
  (let ((images (loop for a across text collect (format nil "cp437-~x" (char-code a)))))
    (loop for image in images for i from 0 do
         (destructuring-bind (cw ch) *cell-dimensions*
           (draw-image image 
                       (* cw (+ x i))
                       (* y ch)
                       cw ch r g b)))))