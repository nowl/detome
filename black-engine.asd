;;; -*- lisp -*-

(defsystem black-engine
  :description "The Black Engine"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on (lispbuilder-sdl)
  :components
  ((:module "black"
			:components
			((:file "package")
			 (:file "globals")
			 (:file "object")
			 (:file "message")
			 (:file "render"))
			:serial t)))
