(in-package #:black)

(export '(perlin2d
          *seed*))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *seed* 0)
(declaim (fixnum *seed*))

;; table of 256 randomly generated noise values between 0 and 255
(defparameter *noise-hash*
  #(208 34 231 213 32 248 233 56 161 78 24 140 71 48 140 254 245 255 247 247 40
    185 248 251 245 28 124 204 204 76 36 1 107 28 234 163 202 224 245 128 167 204
    9 92 217 54 239 174 173 102 193 189 190 121 100 108 167 44 43 77 180 204 8 81
    70 223 11 38 24 254 210 210 177 32 81 195 243 125 8 169 112 32 97 53 195 13
    203 9 47 104 125 117 114 124 165 203 181 235 193 206 70 180 174 0 167 181 41
    164 30 116 127 198 245 146 87 224 149 206 57 4 192 210 65 210 129 240 178 105
    228 108 245 148 140 40 35 195 38 58 65 207 215 253 65 85 208 76 62 3 237 55 89
    232 50 217 64 244 157 199 121 252 90 17 212 203 149 152 140 187 234 177 73 174
    193 100 192 143 97 53 145 135 19 103 13 90 135 151 199 91 239 247 33 39 145
    101 120 99 3 186 86 99 41 237 203 111 79 220 135 158 42 30 154 120 67 87 167
    135 176 183 191 253 115 184 21 233 58 129 233 142 39 128 211 118 137 139 255
    114 20 218 113 154 27 127 246 250 1 8 198 250 209 92 222 173 21 88 102 219))
(declaim ((simple-vector 256) *noise-hash*))

;; A hash that returns a unique value based on the *noise-hash* and
;; the *seed* value for each x and y.
(declaim (inline noise2))
(defun noise2 (x y)
  (declare (fixnum x y))
  (let ((tmp (svref *noise-hash* (mod (+ y *seed*) 256))))
    (declare (fixnum tmp))
    (svref *noise-hash* (mod (+ tmp x) 256))))

;; linear interpolation
(declaim (inline lin-inter))
(defun lin-inter (x y s)
  (declare (single-float x y s))
  (the single-float (+ x (* s (- (float y) (float x))))))

;; smoother interpolation
(declaim (inline smooth-inter))
(defun smooth-inter (x y s)
  (declare (single-float x y s))
  (the single-float (lin-inter x y (* s s (- 3 (* 2 s))))))

;; An "interpolated" noise based on the four integer points
;; surrounding the the desired 2d point.
(defun noise2d (x y)
  (declare (single-float x y))
  (multiple-value-bind (x0 x0frac) (truncate x)
    (declare (fixnum x0))
    (multiple-value-bind (y0 y0frac) (truncate y)
      (declare (fixnum y0))
      (let ((x1 (1+ x0))
            (y1 (1+ y0)))
        (let ((a (noise2 x0 y0))
              (b (noise2 x1 y0))
              (c (noise2 x0 y1))
              (d (noise2 x1 y1)))
          (declare (fixnum a b c d))
          (let ((low (smooth-inter (float a) (float b) x0frac))
                (high (smooth-inter (float c) (float d) x0frac)))
            (the single-float (smooth-inter low high y0frac))))))))

;; Repeatedly calls noise2d with attenuating amplitudes and
;; frequencies based on the depth.
(defun perlin2d (x y freq depth)
  (declare ((or single-float fixnum) x y freq)
           (fixnum depth))
  (let ((xn (float (* x freq)))
        (yn (float (* y freq)))
        (final 0.0)
        (amp 1.0)
        (div 0.0))
    (declare (single-float xn yn final amp div))
    (loop for d below depth do
         (let ((val (noise2d xn yn)))
           (declare (single-float val))
           (incf div (* 256 amp))
           (incf final (* amp val))
           (setf xn (* 2 xn)
                 yn (* 2 yn)
                 amp (/ amp 2))))
    (/ final div)))

(defun perlin2d-grid (width height freq depth)
  (declare (fixnum height width))
  (let ((res (make-array (list height width))))
    (loop for i below height do
         (loop for j below width do
              (setf (aref res i j)
                    (perlin2d j i freq depth))))
    res))