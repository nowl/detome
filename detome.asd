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
   (:file "maps")
   (:file "monster")
   (:file "images")
   (:file "map-cells")
   (:file "hovers")
   (:file "textarea")
   (:file "events")
   (:file "player")
   (:file "level")
   (:file "monster-types")
   (:file "control")
   (:file "dungeon"))
  :serial t)
