(defpackage :black-engine
  (:nicknames #:black)
  (:use #:cl 
		#:lispbuilder-sdl)
  (:export #:test
		   #:*screen-width*
		   #:*screen-height*))