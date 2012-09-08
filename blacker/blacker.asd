;;; -*- lisp -*-

(defsystem blacker
  :description "Blacker Engine"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on ("lispbuilder-sdl"
               "lispbuilder-sdl-image"
               "cl-opengl")
  :components
  ((:file "package")
   (:file "log")
   (:file "engine"))
  :serial t)
