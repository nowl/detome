(in-package #:blacker)

(defparameter *image-cache* (make-hash-table :test #'equal))
(defparameter *tile-cache* (make-hash-table :test #'equal))

(defun clear-image-caches ()
  (setf *image-cache* (make-hash-table :test #'equal))
  (setf *tile-cache* (make-hash-table :test #'equal)))

(defun surface-to-texture (image)
  (declare (optimize (safety 0))
           (sdl-surface image))
  ;; make sure width and height are powers of two
  (assert (= (logand (width image) (1- (width image))) 0))
  (assert (= (logand (height image) (1- (height image))) 0))

  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    
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

(defun get-image (name)
  (declare (simple-string name))
  (gethash name *tile-cache*))

(defun draw-image-at (image x y w h)
  (declare (optimize (safety 0))
           ((or string fixnum) image)
           (real x y w h))
  (gl:bind-texture :texture-2d (etypecase image
                                 (fixnum image)
                                 (string (get-image image))))
  (gl:color 1 1 1)
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
    (gl:vertex x (+ y h))))
