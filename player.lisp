(in-package #:detome)

(defparameter *player* nil)
(defun reset-player ()
  (setf *player* (make-instance 'player
                                :name "player"
                                :x 0
                                :y 0
                                :provisions 1000
                                :g-energy 0
                                :b-energy 0
                                :r-energy 0
                                :y-energy 0
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
                  ;;(textarea-log `("hit " (:color "00ff00") 
                  ;;                       ,(name (mon-type actor2)) (:color "ffffff") " for " (:color "0000ff") 
                  ;;                       ,dmg-txt
                  ;;                       (:color "ffffff") " damage"))
                  (add-damage-hover actor2 dmg-txt "0000ff")
                  (decf (hp actor2) dmg))
                 (t ;;(textarea-log `((:color "ff0000") "missed "
                    ;;                (:color "00ff00") ,(name (mon-type actor2))))
                    (add-damage-hover actor2 "missed" "00007f")))))
        ((eq actor2 *player*)
         ;; mob attacking player
         (let ((dmg-txt (format nil "~d" (round dmg))))
           (cond ((> dmg 0)
                  ;;(textarea-log `((:color "00ff00") ,(name (mon-type actor1)) (:color "ffffff") " hits you for "
                  ;;                (:color "ff0000") 
                  ;;                ,dmg-txt
                  ;;               (:color "ffffff") " damage"))
                  (add-damage-hover actor2 dmg-txt "ff0000")
                  (decf (hp actor2) dmg))
                 (t ;;(textarea-log `((:color "00ff00") ,(name (mon-type actor1))
                    ;;                (:color "ffffff") " misses you"))
                  ;;(add-damage-hover actor2 "missed" "7f0000"))))))
                  nil))))))
         
  
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

(make-object
 :name "player health regen"
 :update-cb #'(lambda (obj)
                ;; only regen health when provisions are positive
                (when (plusp (provisions *player*))
                  (let ((hp-gain (random 1.0)))
                    (setf (hp *player*) (min (+ (hp *player*) hp-gain)
                                             (hp-max *player*))))))
 :update-cb-control '(:turns 1))

(make-object
 :name "player provision use"
 :update-cb #'(lambda (obj)
                (setf (provisions *player*) (max 0 (1- (provisions *player*)))))
 :update-cb-control '(:turns 1))


(make-object
 :name "location renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "LOC: ~d ~d" (x *player*) (y *player*))
                                         (first *location-placement*)
                                         (second *location-placement*)
                                         :color (sdl:color :r #x7f :g #x7f :b #x7f)
                                         :font *larger-font*)))


(make-object
 :name "attack renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "ATK: ~d - ~d"
                                                 (first (att-r *player*))
                                                 (second (att-r *player*)))
                                         (first *attack-placement*)
                                         (second *attack-placement*)
                                         :color (sdl:color :r #x7f :g 0 :b 0)
                                         :font *larger-font*)))

(make-object
 :name "damage renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "DMG: ~d - ~d"
                                                 (first (dmg-r *player*))
                                                 (second (dmg-r *player*)))
                                         (first *damage-placement*)
                                         (second *damage-placement*)
                                         :color (sdl:color :r #x7f :g 0 :b #x7f)
                                         :font *larger-font*)))


(make-object
 :name "defense renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "DFS: ~d - ~d"
                                                 (first (def-r *player*))
                                                 (second (def-r *player*)))
                                         (first *defense-placement*)
                                         (second *defense-placement*)
                                         :color (sdl:color :r 0 :g 0 :b #xaf)
                                         :font *larger-font*)))


(make-object
 :name "provision renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "PROVISIONS: ~d" (provisions *player*))
                                         (first *provision-placement*)
                                         (second *provision-placement*)
                                         :color (sdl:color :r #xaf :g #xaf :b 0)
                                         :font *larger-font*)))

(make-object
 :name "g energy renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "G: ~d" (g-energy *player*))
                                         (first *g-energy-placement*)
                                         (second *g-energy-placement*)
                                         :color (sdl:color :r 0 :g #xaf :b 0)
                                         :font *larger-font*)))

(make-object
 :name "b energy renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "B: ~d" (b-energy *player*))
                                         (first *b-energy-placement*)
                                         (second *b-energy-placement*)
                                         :color (sdl:color :r 0 :g 0 :b #xaf)
                                         :font *larger-font*)))


(make-object
 :name "r energy renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "R: ~d" (r-energy *player*))
                                         (first *r-energy-placement*)
                                         (second *r-energy-placement*)
                                         :color (sdl:color :r #xaf :g 0 :b 0)
                                         :font *larger-font*)))


(make-object
 :name "y energy renderer"
 :render-level "hud"
 :render-cb #'(lambda (obj)
                (sdl:draw-string-solid-* (format nil "Y: ~d" (y-energy *player*))
                                         (first *y-energy-placement*)
                                         (second *y-energy-placement*)
                                         :color (sdl:color :r #xaf :g #xaf :b 0)
                                         :font *larger-font*)))




