;;; -*- lisp -*-

(defsystem detome
  :description "Detome game"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on (blackfish)
  :components
  ((:file "package")
   (:file "globals")
   (:file "message")
   (:file "images")
   (:file "actors")
   (:file "textarea")
   (:file "items")
   (:file "map-cells")
   (:file "hovers")
   (:file "player")
   (:file "dungeon")
   (:file "world-macros")
   (:file "monster")
   (:file "maps")
   (:file "monster-types")
   (:module "world"
            :components
            ((:file "rat-basement")
             (:file "open-plains")
             (:file "random-cave"))
            :serial t)
   (:file "events")
   (:file "process-message")
   (:file "main"))
  :serial t)