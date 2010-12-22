(in-package #:black)

(export '(get-image
          define-image
          clear-image-caches))

(defun clear-image-caches ()
  (setf *image-cache* (make-hash-table :test #'equal))
  (setf *tile-cache* (make-hash-table :test #'equal)))

(defvar *image-cache* (make-hash-table :test #'equal))
(defvar *tile-cache* (make-hash-table :test #'equal))

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
    (let ((clipped-image (sdl:create-surface (width image) (height image) :pixel-alpha t)))
      (blit-surface image clipped-image)
      (setf (gethash name *tile-cache*) clipped-image))))

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
  (let* ((cached-name (format nil "~a-~,1f" name darken))
         (image-orig (gethash name *tile-cache*))
         (mod-image (gethash cached-name *tile-cache*)))
    (unless image-orig
      (error "Unknown image name \"~a\"" name))
    (unless mod-image
      (setf mod-image (darken-surface image-orig darken)
        	(gethash cached-name *tile-cache*) mod-image))
    mod-image))