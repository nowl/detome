(in-package #:detome)

(defparameter *player* nil)
(defun reset-player ()
  (setf *player* (make-instance 'player
                                :name "player"
                                :x 1
                                :y 1
                                :hp 25
                                :hp-max 25
                                :att-r '(4 10)
                                :dmg-r '(1 4)
                                :def-r '(1 5))))

(defun rand (max &optional (min 0))
  (let ((diff (- max min))
        (r (random 1.0)))
    (if (<= diff 0) 
        max
        (+ min (* r diff)))))

(defun hit (actor1 actor2 dmg)
  (cond ((eq actor1 *player*)
         ;; player attacking mob
         (let ((dmg-txt (format nil "~d" (round dmg))))
           (cond ((> dmg 0)
                  (textarea-log `("hit " (:color "00ff00") 
                                         ,(name (mon-type actor2)) (:color "ffffff") " for " (:color "0000ff") 
                                         ,dmg-txt
                                         (:color "ffffff") " damage"))
                  (add-damage-hover actor2 dmg-txt "0000ff")
                  (decf (hp actor2) dmg))
                 (t (textarea-log `((:color "ff0000") "missed "
                                    (:color "00ff00") ,(name (mon-type actor2))))
                    (add-damage-hover actor2 "missed" "00007f")))))
        ((eq actor2 *player*)
         ;; mob attacking player
         (let ((dmg-txt (format nil "~d" (round dmg))))
           (cond ((> dmg 0)
                  (textarea-log `((:color "00ff00") ,(name (mon-type actor1)) (:color "ffffff") " hits you for "
                                  (:color "ff0000") 
                                  ,dmg-txt
                                  (:color "ffffff") " damage"))
                  (add-damage-hover actor2 dmg-txt "ff0000")
                  (decf (hp actor2) dmg))
                 (t (textarea-log `((:color "00ff00") ,(name (mon-type actor1))
                                    (:color "ffffff") " misses you"))
                    (add-damage-hover actor2 "missed" "7f0000")))))))
         
  
(defun attack (actor1 actor2)
  (destructuring-bind (attmin attmax) (att-r actor1)
    (destructuring-bind (defmin defmax) (def-r actor2)
      (destructuring-bind (dmgmin dmgmax) (dmg-r actor1)
        (let* ((a1-att (rand attmax attmin))
               (a1-def (rand defmax defmin))
               (dmg (rand dmgmax dmgmin)))
          (hit actor1 actor2 (if (> a1-att a1-def) dmg 0)))))))

(defun draw-player ()  
  (multiple-value-bind (x y) (get-screen-pos-of *player*)
    (sdl:draw-surface-at-* (get-image "player-front") x y)))

(make-object
 :name "health renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* "HP:"
                                         (first *health-placement*)
                                         (second *health-placement*)
                                         :color (sdl:color :r #xff :g 0 :b 0)
                                         :font *larger-font*)
                (let* ((start (+ (* 4 *larger-font-width*) (first *health-placement*)))
                       (width (third *health-placement*)))
                  (sdl:draw-box-* start
                                  (second *health-placement*)
                                  width
                                  *larger-font-height*
                                  :color (sdl:color :r #x5f :g 0 :b 0))
                  (sdl:draw-box-* start
                                  (second *health-placement*)
                                  (max (floor (* width (/ (hp *player*) (hp-max *player*)))) 0)
                                  *larger-font-height*
                                  :color (sdl:color :r #xff :g 0 :b 0)))))
