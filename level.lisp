(in-package #:detome)

(defvar *level*
  (make-array '(50 50)
              :initial-contents 
              '((0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 1 1 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defvar *intensity-map* (funcall #'make-array (array-dimensions *level*)))
(defvar *explored-map* (funcall #'make-array (array-dimensions *level*)))

(defvar *player* (make-instance 'player :x 1 :y 1))

(defvar *map-cells-by-number* (make-hash-table :test #'eq))
(defvar *map-cells-by-name* (make-hash-table :test #'equal))

(defstruct map-cell
  number
  name
  attenuation
  walkable
  image)

(defmacro define-map-cell (number name &key attenuation walkable image)
  (let ((cell (gensym)))
	`(let ((,cell (make-map-cell :number ,number
                        :attenuation ,attenuation
						:walkable ,walkable
                        :image ,image)))
	   (setf (gethash ,number *map-cells-by-number*)
			 ,cell
			 (gethash ,name *map-cells-by-name*)
			 ,cell))))

(defun define-images ()
  (define-image "plain" "data/tileset.png" '(1 1 32 32))
  (define-image "wall" "data/tileset.png" '(34 1 32 32))
  (define-image "mountain" "data/tileset.png" '(67 1 32 32))
  (define-image "player-front" "data/tileset.png" '(1 34 32 32)))

(define-map-cell 0
	"plain"
    :attenuation '(0.1 :dark 0.6)
    :walkable t
    :image "plain")
(define-map-cell 1
	"wall"
    :attenuation 1.0
    :walkable nil
    :image "wall")
(define-map-cell 2
	"mountain"
    :attenuation '(0.75 :dark 0.9)
    :walkable t
    :image "mountain")

(defvar *message-area-rawtext* nil)
(defvar *message-area-buffer* nil)
(defun textarea-log (message-as-list 
		     &key (location-func #'(lambda (log) (push log *message-area-rawtext*))) 
		     (ttl *default-message-ttl-sec*))
  (funcall location-func (cons ttl (list (append `((:color ,sdl:*white*)) message-as-list)))))

(defvar *hover-messages* nil
  "Hover messages is a list of hover messages in progress.")
(defstruct hover
  x y
  mover
  width height
  box-color alpha
  draw-rect
  formatted-strings)
(defun make-hover-message (x y width color alpha message-as-list &key mover (draw-rect t) fit-height (height black::*screen-height*))
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
				 :formatted-strings strings)
     *hover-messages*)))

(defun attenuation-lookup (x y map)
  (let* ((map-point (aref map y x))
         (att (map-cell-attenuation (gethash map-point *map-cells-by-number*))))
    (etypecase att
      (float att)
      (cons (let ((res (member *weather* att)))
              (if res
                  (second res)
                  (first att)))))))

(defun clear-intensity-map ()
  (loop for y below (array-dimension *intensity-map* 0) do
       (loop for x below (array-dimension *intensity-map* 1) do
            (setf (aref *intensity-map* y x) 0.0))))

(defun clear-explored-map ()
  (loop for y below (array-dimension *explored-map* 0) do
       (loop for x below (array-dimension *explored-map* 1) do
            (setf (aref *explored-map* y x) 0.0))))

(defun update-intensity-map (x y intensity)
  (clear-intensity-map)
  (setf (aref *intensity-map* y x) intensity)
  (let ((sights (line-of-sight x y
                               (array-dimension *level* 0)
                               (array-dimension *level* 1)
                               #'(lambda (x y) (funcall #'detome::attenuation-lookup x y detome::*level*)) 
			       *light-intensity-cutoff*)))
    (loop for sight in sights do
         (destructuring-bind (x y rel-intensity) sight
           (setf (aref *intensity-map* y x)
                 (* intensity rel-intensity)
                 (aref *explored-map* y x)
                 0.25)))))

(update-intensity-map (x *player*) (y *player*) 1.0)


;; TODO remove this
(setf black::*event-list-quit-event* nil)

(add-quit-event #'common-quit-event)

;; TODO remove this
(setf black::*event-list-key-down-event* nil)

(defun press-escape-to-quit-event (&key key &allow-other-keys)
  (cond ((sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event)
         t)
        (t nil)))

(add-key-down-event #'press-escape-to-quit-event)

(defun move-map-window-if-needed ()
  ;; The map window should stay relatively centered on the player
  ;; unless we are near an edge in which case the map window fills the
  ;; view.

  (let ((level-height (array-dimension *level* 0))
	(level-width (array-dimension *level* 1))
	(window-width (nth 2 *map-window*))
	(window-height (nth 3 *map-window*)))

    ;; handle x direction
    (cond ((< (x *player*) (/ window-width 2))
	   (setf (nth 0 *map-window*) 0))
	  ((> (x *player*) (- level-width (/ window-width 2)))
	   (setf (nth 0 *map-window*) (- level-width window-width)))
	  (t (setf (nth 0 *map-window*) (- (x *player*) (/ window-width 2)))))

    ;; handle y direction
    (cond ((< (y *player*) (/ window-height 2))
	   (setf (nth 1 *map-window*) 0))
	  ((> (y *player*) (- level-height (/ window-height 2)))
	   (setf (nth 1 *map-window*) (- level-height window-height)))
	  (t (setf (nth 1 *map-window*) (- (y *player*) (/ window-height 2)))))))

(defun walkable (x y)
  (let ((level-width (array-dimension *level* 1))
	(level-height (array-dimension *level* 0)))
    (unless (and (>= x 0) (>= y 0) (< x level-width) (< y level-height))
      (return-from walkable nil)))      
  (let* ((map-point (aref *level* y x)))
    (map-cell-walkable (gethash map-point *map-cells-by-number*))))

(defun attempt-move-player (delta-x delta-y)
  (with-slots (x y) *player*
    (let ((new-x (+ x delta-x))
	  (new-y (+ y delta-y)))
      (if (walkable new-x new-y)
	  (setf x new-x y new-y)
	  (textarea-log `("Blocked going " (:color "0000ff") 
					   ,(ecase delta-x
						   (1 (ecase delta-y
							(0 "east")
							(1 "southeast")
							(-1 "northeast")))
						   (0 (ecase delta-y
							(1 "south")
							(-1 "north")))
						   (-1 (ecase delta-y
							 (1 "southwest")
							 (0 "west")
							 (-1 "northwest"))))
					   (:color ,sdl:*white*) "!"))))))

(defun add-damage-hover (actor amount)
  (declare (actor actor))
  (let ((text `((:color "ff0000") ,(write-to-string amount)))
		(x-y (multiple-value-list (get-screen-pos-of actor))))
    (make-hover-message (nth 0 x-y) (nth 1 x-y) 100 "ff0000" #xa0 text :mover (list nil -4) :draw-rect nil :fit-height t)))

(defun add-health-hover ()
  (let ((text '((:color "ffffff") "This is a transparent hover test. Your hit points, mana, etc. will appear here. In " (:color "ff0000") "color!")))
    (make-hover-message 10 10 (- (* 32 (nth 2 *map-window*)) 10) "00ffff" #x80 text :mover (list nil 4) :draw-rect nil :fit-height t)))

(defun scroll-map-with-arrows-event (&key key &allow-other-keys)
  (cond ((sdl:key= key :sdl-key-a)
         (make-and-send-message 
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
		      (attempt-move-player -1 0)
		      (move-map-window-if-needed)
                      (update-intensity-map (x *player*) (y *player*) 1.0)))
         t)
        ((sdl:key= key :sdl-key-d)
         (make-and-send-message 
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
		      (attempt-move-player 1 0)
		      (move-map-window-if-needed)
                      (update-intensity-map (x *player*) (y *player*) 1.0)))
         t)
        ((sdl:key= key :sdl-key-s)
         (make-and-send-message 
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
		      (attempt-move-player 0 1)
		      (move-map-window-if-needed)
                      (update-intensity-map (x *player*) (y *player*) 1.0)))
         t)
        ((sdl:key= key :sdl-key-w)
         (make-and-send-message 
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
		      (attempt-move-player 0 -1)
		      (move-map-window-if-needed)
			  (update-intensity-map (x *player*) (y *player*) 1.0)))
         t)
		((sdl:key= key :sdl-key-r)
         (make-and-send-message
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
					  (mid-displace 10 10 :array *level* :roughness 100.0 
									:post-filter-func #'(lambda (val)
														  (if (> val 0.75)
															  (map-cell-number (gethash "mountain" *map-cells-by-name*))
															  (map-cell-number (gethash "plain" *map-cells-by-name*)))))
					  (update-intensity-map (x *player*) (y *player*) 1.0)))
         t)
		((sdl:key= key :sdl-key-h)
         (make-and-send-message 
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
					  (setf (update-cb-control (get-object-by-name "stat remover"))
			    '(:seconds 4.0))
		      (add-health-hover)))
         t)
		((sdl:key= key :sdl-key-j)
         (make-and-send-message
          :sender "event processor" :receiver "global message receiver"
          :action #'(lambda (sender receiver)
					  (add-damage-hover *player* -5)))
					   ;(setf (update-cb-control (get-object-by-name "stat remover"))
					;		 '(:seconds 4.0))
					;   (add-health-hover)))
         t)
        (t nil)))


(add-key-down-event #'scroll-map-with-arrows-event)

(defmacro clip (value a b)
  `(if (<= ,value ,a)
       ,a
       (if (>= ,value ,b)
           ,b
           ,value)))

(defun los-intensity-at-point (x y)
  (aref *intensity-map* y x))

(defun total-intensity-at-point (x y)
  (let ((map-int (los-intensity-at-point x y)))
    (if (plusp map-int)
	;; if the LOS includes this point then only return it's intensity
	map-int
	;; otherwise return the explored map intensity
	(aref *explored-map* y x))))

(defun image-from-map (x y)
  (let ((darken-amount (clip (- 1 (total-intensity-at-point x y))
                             0.0 1.0)))
    (let ((map-point (aref *level* y x)))
      (get-image (map-cell-image (gethash map-point *map-cells-by-number*))
                 :darken darken-amount))))

(defgeneric get-screen-pos-of (obj)
  (:documentation 
   "Returns the screen position of the given object as x and y
    values"))

(defmethod get-screen-pos-of ((obj actor))
  (values (* (- (x obj) (first *map-window*)) 32)
	  (* (- (y obj) (second *map-window*)) 32)))


(defun draw-player (interpolation)  
  (multiple-value-bind (x y) (get-screen-pos-of *player*)
  (sdl:draw-surface-at-* (get-image "player-front") x y)))

(defun draw-background (interpolation)
  (destructuring-bind (map-width map-height) (array-dimensions *level*)
    (loop for x from (max (first *map-window*) 0) below (min map-width
                                                             (+ (third *map-window*)
                                                                (first *map-window*))) do
         (loop for y from (max (second *map-window*) 0) below (min map-height
                                                                   (+ (fourth *map-window*)
                                                                      (second *map-window*))) do
              (sdl:draw-surface-at-* (image-from-map x y)
				     
                                     (* (- x (first *map-window*)) 32)
                                     (* (- y (second *map-window*)) 32))))))

(defvar *message-area-strings* nil)

(defun draw-message-textarea (strings interpolation)
  (let (pos-x pos-y (color (sdl:color :r #xff :g #xff :b #xff)))
    (loop for message in strings do
	 (etypecase message
	   (cons (ecase (car message)
		   (:render-at (setf pos-x (second message)
				     pos-y (third message)))
		   (:color (setf color (second message)))))
	   (string
	    (sdl:draw-string-solid-* message
				     pos-x
				     pos-y
				     :color color
				     :font *primary-font*))))))

(defun draw-hover-messages (interpolation)
  (loop for hover in *hover-messages* do
	   (draw-message-textarea (hover-formatted-strings hover) interpolation)
	   (when (hover-draw-rect hover)
		 (let ((rect-surf (sdl:create-surface (hover-width hover) (hover-height hover) :alpha (hover-alpha hover))))
		   (sdl:flood-fill-* 0 0 :surface rect-surf :color (hover-box-color hover))
		   (sdl:draw-surface-at-* rect-surf (hover-x hover) (hover-y hover))))))

(define-object
    :name "primary renderer"
  :render-cb #'(lambda (obj interpolation)
                 (draw-background interpolation)
                 (draw-player interpolation)
		 (draw-hover-messages interpolation)
		 (draw-message-textarea *message-area-strings* interpolation)))


(define-object
    :name "render updater"
  :update-cb #'(lambda (obj)
		 (setf *primary-font* (sdl:initialise-default-font *primary-font-name*))
		 (sdl:enable-alpha t :surface sdl:*default-display*)
		 (sdl:enable-alpha t :surface sdl:*default-surface*)
		 (sdl:disable-key-repeat)
		 (sdl:enable-key-repeat 200 50)
		 (define-images)         
		 (clear-explored-map)
		 (clear-render-list)		 
		 (add-to-render-list "primary renderer"))
  :update-cb-control :one-shot)

(define-object
    :name "message textarea updater"
  :update-cb #'(lambda (obj)
				 (multiple-value-setq (*message-area-strings* *message-area-buffer*)
				   (update-message-strings
					(second (black::update-cb-control obj))
					*message-area-buffer*
					:rawtext *message-area-rawtext*))
				 (setf *message-area-rawtext* nil))
  :update-cb-control '(:seconds 0.1))


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
    :name "weather builder"
  :update-cb #'(lambda (obj)
		 (ecase *environment*
		   (:outside (cond ((eq *weather* :clear)
				    (setf *weather* :dark)
				    (textarea-log '("You feel the clastrophobic oppression of darkness."))
				    (update-intensity-map (x *player*) (y *player*) 1.0))
				   ((eq *weather* :dark)
				    (setf *weather* :clear)
				    (textarea-log '("The shadows become slightly less threatening as night becomes day."))
				    (update-intensity-map (x *player*) (y *player*) 1.0))))))
  :update-cb-control `(:seconds ,*day-night-cycle-in-seconds*))

(define-object :name "event processor")

(define-object :name "global message receiver"
  :update-cb-control '(:ticks 1))

(defvar *stats-visible* t)
(define-object
    :name "stat remover"
  :update-cb #'(lambda (obj)
		 ;; TODO fix this so it only removes the stats hover
		 ;; message not all of them
		 (setf *hover-messages* nil)
		 (setf (update-cb-control obj) :none))
  :update-cb-control :none)
		 
(defun detome ()
  (textarea-log '("Welcome to " (:color "ff0000") "Detome" (:color "ffffff") "! The goal of this game is to hunt down the dark wizard Varlok and have some good looting fun on the way.")
		:ttl 20)
  (mainloop))
