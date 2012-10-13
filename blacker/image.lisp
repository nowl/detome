(in-package #:blacker)

(export '(clear-image-caches
          define-image
          get-image
          draw-image
          draw-line))

(defparameter *image-cache* (make-hash-table :test #'equal))
(defparameter *tile-cache* (make-hash-table :test #'equal))

(defun clear-image-caches ()
  (setf *image-cache* (make-hash-table :test #'equal))
  (setf *tile-cache* (make-hash-table :test #'equal)))

(defun surface-to-texture (image)
  ;; make sure width and height are powers of two
  ;(assert (= (logand (width image) (1- (width image))) 0))
  ;(assert (= (logand (height image) (1- (height image))) 0))

  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
    
    (with-pixel (pix (sdl:fp image))
      ;; we should probably be a bit more intelligent about this, but this
      ;; handles some common cases
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (3 (if (= (r-mask image) #xff)
                                     :rgb
                                     :bgr))
                              (4 (if (= (r-mask image) #xff)
                                     :rgba
                                     :bgra)))))
        ;; we should also handle this properly, by adjusting the
        ;; settings of gl:pixel-store
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    texture))

(defun define-image (name path clipping-rectangle)
  (declare (simple-string name path)
           ((cons fixnum (cons fixnum (cons fixnum (cons fixnum)))) clipping-rectangle))
  (let ((image (gethash name *image-cache*)))
    (unless image
      (setf image (sdl-image:load-image path)
            (gethash name *image-cache*) image))
    (sdl:set-cell-* (first clipping-rectangle)
                    (second clipping-rectangle)
                    (third clipping-rectangle)
                    (fourth clipping-rectangle)
                    :surface image)
    (let ((clipped-image (sdl:create-surface (third clipping-rectangle) (fourth clipping-rectangle) :pixel-alpha t)))
      (blit-surface image clipped-image)
      (setf (gethash name *tile-cache*) (surface-to-texture clipped-image)))))

(defun darken-surface (surface amount)
  (declare (sdl:surface surface)
           (short-float amount))
  (let ((new-surface (create-surface 32 32 :pixel-alpha t)))
    ;; XXX: in the past we had the locked surface version here, but
    ;;   this seems to not be required in the newer svn versions of
    ;;   lispbuilder-sdl
    ;;(with-locked-surface (surf new-surface)
    (let ((surf new-surface))
      (loop for y below (height surf) do
           (loop for x below (width surf) do
                (let ((old-pixel-color (sdl:read-pixel-* x y :surface surface)))
                  (draw-pixel-* x y 
                                :surface surf
                                :color (color :r (* (- 1 amount) (r old-pixel-color))
                                              :g (* (- 1 amount) (g old-pixel-color))
                                              :b (* (- 1 amount) (b old-pixel-color))
                                              :a (a old-pixel-color)))))))
    new-surface))

(defun get-image (name &key (darken 0.0))
  (declare (simple-string name)
           (short-float darken))
  (gethash name *tile-cache*))

(defun draw-image (name x y w h r g b)
  (let ((image (get-image name)))
    (gl:bind-texture :texture-2d image)
    (gl:color r g b)
    (gl:enable :texture-2d)
    ;;(gl:enable :blend)
    ;;(gl:blend-func :src-alpha :one-minus-src-alpha)

    (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex x y)
      (gl:tex-coord 1 0)
      (gl:vertex (+ x w) y)
      (gl:tex-coord 1 1)
      (gl:vertex (+ x w) (+ y h))
      (gl:tex-coord 0 1)
      (gl:vertex x (+ y h)))))

;; general drawing routines

(defparameter *line-width* 1)

(defun draw-line (xs ys xe ye r g b)
  (gl:disable :texture-2d)
  (gl:color r g b)
  (gl:line-width *line-width*)

  (gl:with-primitive :lines
    (gl:vertex xs ys)
    (gl:vertex xe ye)))