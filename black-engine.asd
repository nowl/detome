;;; -*- lisp -*-

(defsystem black-engine
  :description "The Black Engine"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on (lispbuilder-sdl
               lispbuilder-sdl-image)
  :components
  ((:module "black"
	    :components
	    ((:file "package")
	     (:file "log")
	     (:file "globals")
	     (:file "object")
	     (:file "message")
	     (:file "render")
	     (:file "update")
	     (:file "events")
	     (:file "common")
	     (:file "image")
	     (:file "macros")
	     (:file "los")
	     (:file "main")
	     (:module "misc"
		      :components
		      ((:file "mid-disp")
		       (:file "random"))
		      :serial t))
	    :serial t)))
  