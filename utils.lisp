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

(defun break-up-text (text size)
  (labels ((find-space (txt start)
             (let ((new-pos (position #\  txt :start start)))
               (when new-pos (cons new-pos (find-space txt (1+ new-pos))))))
           (find-seq-pos (space-positions)
             (loop with done for i from 1 while (not done) collect
                  (let* ((mod (* i size))
                         (pos (position-if #'(lambda (x) (>= x mod))
                                           space-positions)))
                    (if pos (1- pos) (progn (setf done t) nil))))))
    (let ((space-positions (append (find-space text 0) (list (1+ (length text))))))
      ;;(format t "positions ~a~%" space-positions)
      (loop with prev = 0 for pos in (find-seq-pos space-positions) collect
           (let ((space-pos (if pos
                                (nth pos space-positions)
                                (length text))))
             (prog1 (subseq text prev space-pos)
               ;;(format t "pos ~a~%" pos)
               (setf prev (1+ space-pos))))))))
           
(defun draw-text (text x y r g b &key fit-to)
  (let ((broken-text (if fit-to
                         (break-up-text text fit-to)
                         (list text))))
      (loop for j below (length broken-text) do
           (let* ((text-line (nth j broken-text))
                  (images (loop for a across text-line collect (format nil "cp437-~x" (char-code a)))))
             (loop for image in images for i from 0 do
                  (destructuring-bind (cw ch) *cell-dimensions*
                    (draw-image image 
                                (* cw (+ x i))
                                (* (+ y j) ch)
                                cw ch r g b)))))))