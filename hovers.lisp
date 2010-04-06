(in-package #:detome)

(defvar *hover-messages* nil
  "Hover messages is a list of hover messages in progress.")

(defstruct hover
  x y
  mover
  width height
  box-color alpha
  draw-rect
  ttl
  formatted-strings)

(defun make-hover-message (x y width color alpha message-as-list &key mover ttl (draw-rect t) fit-height (height black::*screen-height*))
  (let ((raw-message (list (cons 1 (list (append `((:color ,sdl:*white*)) message-as-list)))))
        strings
        buffer)
    (multiple-value-setq (strings buffer)
      (update-message-strings
       0.0
       buffer
       :rawtext raw-message
       :message-textarea-window (list x y width height)))
    ;; fix height if needed
    (when fit-height
      (let (min max)
        (loop for x in strings do
             (if (and (consp x) (eq (first x) :render-at))
                 (let ((h-cand (third x)))
                   (cond ((null min) (setf min h-cand))
                         ((null max) (setf max h-cand))
                         (t (when (< h-cand min) (setf min h-cand))
                            (when (> h-cand max) (setf max h-cand)))))))
        (setf height (+ (- max min) *primary-font-height* (* 2 *message-textarea-height-offset-between-messages*)))))
    (push
     (make-hover :x x
                 :y y
                 :mover mover
                 :width width
                 :height height
                 :box-color (etypecase color
                              (string (hex-string-to-color color))
                              (sdl:color color))
                 :alpha alpha
                 :draw-rect draw-rect
                 :ttl ttl
                 :formatted-strings strings)
     *hover-messages*)))

(defun add-damage-hover (actor amount)
  (declare (actor actor))
  (let ((text `((:color "ff0000") ,amount))
        (x-y (multiple-value-list (get-screen-pos-of actor))))
    (make-hover-message (+ 8 (nth 0 x-y)) (nth 1 x-y) 100 "ff0000" #xa0 text :mover (list nil -3) :ttl 8 :draw-rect nil :fit-height t)))

(defun add-health-hover ()
  (let ((text '((:color "ffffff") "This is a transparent hover test. Your hit points, mana, etc. will appear here. In " (:color "ff0000") "color!")))
    (make-hover-message 10 10 (- (* 32 (nth 2 *map-window*)) 10) "00ffff" #x80 text :ttl 100 :mover (list nil 4) :draw-rect nil :fit-height t)))

(defun draw-hover-messages (interpolation)
  (loop for hover in *hover-messages* do
       (draw-message-textarea (hover-formatted-strings hover) interpolation)
       (when (hover-draw-rect hover)
         (let ((rect-surf (sdl:create-surface (hover-width hover) (hover-height hover) :alpha (hover-alpha hover))))
           (sdl:flood-fill-* 0 0 :surface rect-surf :color (hover-box-color hover))
           (sdl:draw-surface-at-* rect-surf (hover-x hover) (hover-y hover))))))

(define-object
    :name "hover mover updater"
  :update-cb #'(lambda (obj)
                 (loop for hover in *hover-messages* do
                      (destructuring-bind (move-x move-y) (hover-mover hover)
                        (loop for string in (hover-formatted-strings hover) do
                             (when (and move-x
                                        (typep string 'cons)
                                        (eq :render-at (first string)))
                               (setf (second string) (+ (second string) move-x)))
                             (when (and move-y
                                        (typep string 'cons)
                                        (eq :render-at (first string)))
                               (setf (third string) (+ (third string) move-y)))))))
  :update-cb-control '(:ticks 1))

(define-object
    :name "hover remover"
  :update-cb #'(lambda (obj)
                 (let (hovers-to-remove)
                   (dolist (hover *hover-messages*)
                     (when (hover-ttl hover)
                       (decf (hover-ttl hover))
                       (and (<= (hover-ttl hover) 0)
                            (push hover hovers-to-remove))))
                   (dolist (hover hovers-to-remove)
                     (setf *hover-messages* (delete hover *hover-messages* :test #'eq)))))
  :update-cb-control '(:ticks 1))
