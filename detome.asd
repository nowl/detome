;;; -*- lisp -*-

(defsystem detome
  :description "Detome game"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on (black-engine)
  :components
  ((:file "package")
   (:file "globals")
   (:file "message")
   (:file "actors")
   (:file "items")
   (:file "map-cells")
   (:file "images")
   (:file "hovers")
   (:file "player")
   (:file "dungeon")
   (:file "world-macros")
   (:file "monster")
   (:file "textarea")
   (:file "events")
   (:file "maps")
   (:file "monster-types")
   (:module "world"
            :components
            ((:file "rat-basement")
             (:file "open-plains")
             (:file "random-cave"))
            :serial t)
   (:file "main"))
  :serial t)
