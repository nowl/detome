(in-package #:detome)

(defvar *player* (make-instance 'player
                                :name "player"
                                :x 1
                                :y 1
                                :hp 10
                                :att-r '(1 5)
                                :dmg-r '(1 4)
                                :def-r '(1 5)))

(defun rand (max &optional (min 0))
  (let ((diff (- max min))
        (r (random 1.0)))
    (if (<= diff 0) 
        max
        (+ min (* r diff)))))

(defun hit (actor1 actor2 dmg)
  (assert (eq actor1 *player*))
  (let ((dmg-txt (format nil "~d" (round dmg))))
    (cond ((> dmg 0)
           (textarea-log `("hit " (:color "00ff00") ,(name (mon-type actor2)) (:color "ffffff") " for " (:color "0000ff") 
                                  ,dmg-txt
                                  (:color "ffffff") " damage"))
           (add-damage-hover actor2 dmg-txt)
           (decf (hp actor2) dmg))
          (t (textarea-log `((:color "ff0000") "missed " (:color "00ff00") ,(name (mon-type actor2))))
             (add-damage-hover actor2 "missed")))))
  
(defun attack (actor1 actor2)
  (destructuring-bind (attmin attmax) (att-r actor1)
    (destructuring-bind (defmin defmax) (def-r actor2)
      (destructuring-bind (dmgmin dmgmax) (dmg-r actor1)
        (let* ((a1-att (rand attmax attmin))
               (a1-def (rand defmax defmin))
               (dmg (rand dmgmax dmgmin)))
          (hit actor1 actor2 (if (> a1-att a1-def) dmg 0)))))))

(defun draw-player (interpolation)  
  (multiple-value-bind (x y) (get-screen-pos-of *player*)
    (sdl:draw-surface-at-* (get-image "player-front") x y)))
