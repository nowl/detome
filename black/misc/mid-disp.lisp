(in-package #:black)

(export 'mid-displace)

(defun mod-round (num)
  (declare (float num))
  "Similar to round except that it always rounds 0.5 to the next
highest integer."
  (floor (+ num 0.5)))

(defun rand-mid (val orig-val v1 v2 v3 v4 roughness)  
  (declare (single-float val orig-val v1 v2 v3 v4 roughness))
  (let* ((max (* roughness (/ val orig-val)))
		 (disp (* (- (random 1.0) 0.5) max))
		 (new (+ (/ (+ v1 v2 v3 v4) 4) disp)))
	(cond ((> new 1.0) 1.0)
		  ((< new 0.0) 0.0)
		  (t new))))

(defun mid-displace-aux (x y width height v1 v2 v3 v4 roughness array)
  (declare (fixnum x y)
		   (single-float width height v1 v2 v3 v4 roughness)
		   (simple-array array))
  (if (or (>= width 1) (>= height 1))
	  (let* ((new-height (float (/ height 2)))
			 (new-width (float (/ width 2)))
			 (midpoint (rand-mid (+ new-width new-height)
								 (float (apply #'+ -2 (array-dimensions array)))
								 v1 v2 v3 v4
								 roughness)))
		(let ((e1 (/ (+ v1 v2) 2))
			  (e2 (/ (+ v2 v3) 2))
			  (e3 (/ (+ v3 v4) 2))
			  (e4 (/ (+ v4 v1) 2)))
		  (mid-displace-aux x y new-width new-height v1 e1 midpoint e4
							roughness array)
		  (mid-displace-aux (mod-round (+ x new-width)) y new-width new-height
							e1 v2 e2 midpoint roughness array)
		  (mid-displace-aux (mod-round (+ x new-width)) 
							(mod-round (+ y new-height))
							new-width new-height
							midpoint e2 v3 e3 roughness array)
		  (mid-displace-aux x (mod-round (+ y new-height)) new-width new-height
							e4 midpoint e3 v4 roughness array)))
	  (setf (aref array y x)
			(/ (+ v1 v2 v3 v4) 4))))

(defun random-normal (&optional (mean 0.0) (variance 1.0))
  (let ((s 1)
		v1 v2)
	(loop with u1 and u2 while (>= s 1) do
		 (setf u1 (random 1.0)
			   u2 (random 1.0)
			   v1 (- (* 2 u1) 1)
			   v2 (- (* 2 u2) 1)
			   s (+ (* v1 v1) (* v2 v2))))
	;; just returning the first value and throwing away the second
	;; seems to be faster than caching it
	(let ((val1 (* v1 (sqrt (/ (* -2 (cl:log s)) s)))))
	  (+ mean (* (sqrt variance) val1)))))

(defun clip (value min max)
  (if (<= value min)
	  min
	  (if (>= value max)
		  max
		  value)))
	 
(defun mid-displace (width height &key (post-filter-func nil) (roughness 1.0) type array)
  "Creates an array sized width and height of floats between 0 and 1
representing a plasma fractal with a given roughness. If an array is
passed in then that array is used and width and height are ignored. If
a post-filter-func is provided then after the array is created then
the function is called with each element in the array and the value is
assigned to what is returned from this function. 'type' represents the
mean and variance of the four starting corners of the map. This
combined with the 'roughness' factor should allow for a wide range of
flexibility and desired behavior."
  (declare ((or integer null) width height)
		   (number roughness)
		   ((or array null) array)
		   ((or null (cons single-float (cons single-float null))) type)
		   ((or function null) post-filter-func))
  (if array
	  ;; use the array passed in
	  (setf width (array-dimension array 1)
			height (array-dimension array 0))
	  ;; create a new array
	  (setf array (make-array (list height width) :element-type 'single-float)))

  ;; set initial corner values
  (let (c1 c2 c3 c4)
	(if type

		;; use mean and variance
		(destructuring-bind (mean var) type
		  (setf c1 (clip (random-normal mean var) 0.0 1.0)
				c2 (clip (random-normal mean var) 0.0 1.0)
				c3 (clip (random-normal mean var) 0.0 1.0)
				c4 (clip (random-normal mean var) 0.0 1.0)))

		;; just use a random uniform prob. from 0 to 1
		(setf c1 (random 1.0)
			  c2 (random 1.0)
			  c3 (random 1.0)
			  c4 (random 1.0)))

	(mid-displace-aux 0 0 (float (1- width)) (float (1- height))
					  c1 c2 c3 c4
					  (float roughness)
					  array))

  ;; apply the post filter if available
  (when post-filter-func
	(loop for h below height do
		 (loop for w below width do
			  (let ((val (aref array h w)))
				(setf (aref array h w)
					  (funcall post-filter-func val))))))
  array)

(defun pgm-out (filename array)
  "Outputs the array of floats to a PGM file"
  (let ((shades (1- (expt 2 16))))
	(with-open-file (out filename :direction :output :if-exists :supersede)
	  (destructuring-bind (height width) (array-dimensions array)
		(format out "P2~%~D ~D~%~D~%" width height shades)
		(loop for h below height do
			 (loop for w below width do
				  (let ((val (aref array h w)))
					(format out "~3D " (round (* val shades)))))
			 (format out "~%"))))))

(pgm-out "test.pgm" (mid-displace 512 512 :roughness 5))