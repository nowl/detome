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
   (:file "monster")
   (:file "mid-disp")
   (:file "level")
   (:file "monster-types")
   (:file "control"))
  :serial t)
