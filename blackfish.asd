;;; -*- lisp -*-

(defsystem blackfish
  :description "The BlackFish Engine"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on ("lispbuilder-sdl"
               "lispbuilder-sdl-image")
  :components
  ((:module "blackfish"
            :components
            ((:file "package")
             (:file "log")
             (:file "game")
             (:file "message")
             (:module "misc"
                      :components
                      ((:file "mid-disp")
                       (:file "perlin")
                       (:file "random")
                       (:file "los")
                       (:file "image"))
                      :serial t))
            :serial t)))
  
