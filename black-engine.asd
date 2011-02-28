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
             (:file "generics")
             (:file "config")
             (:file "object")
             (:file "object-manager")
             (:file "game-state")
             (:file "message")
             (:file "image")
             (:file "macros")
             (:file "los")
             (:file "main")
             (:module "misc"
                      :components
                      ((:file "mid-disp")
                       (:file "perlin")
                       (:file "random"))
                      :serial t))
            :serial t)))
  
