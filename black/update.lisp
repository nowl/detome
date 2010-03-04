(in-package #:black)

(defun update-obj (obj)
  (declare (object obj))
  (with-slots (update-cb update-cb-control) obj
    (flet ((call-update ()
             (process-messages-for-obj obj)
             (when update-cb
               (funcall update-cb obj))))
      (etypecase update-cb-control
        (keyword (ecase update-cb-control
                   (:all (call-update))
                   (:one-shot (call-update)
                              (setf update-cb-control :none))
                   (:none nil)))
        (cons (ecase (first update-cb-control)
                (:ticks (let ((ticks (second update-cb-control)))
                          (when (eql (mod *game-tick* ticks) 0)
                            (call-update))))
                (:turns (let ((num-turns (second update-cb-control)))
                          (when (> num-turns 0)
                            (call-update)
                            (setf (second update-cb-control) (decf num-turns)))))
                (:seconds (let* ((seconds (second update-cb-control))
                                 (modulo (ceiling (/ (* seconds 1000) *ms-per-update*))))
                            (when (eql (mod *game-tick* modulo) 0)
                              (call-update))))))))))