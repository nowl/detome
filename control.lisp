(in-package :detome)

(defun reload-images ()
  (clear-image-caches)
  (setf (update-cb-control (get-object-by-name "render updater")) :one-shot)
  (clear-render-list))