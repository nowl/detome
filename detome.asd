;;; -*- lisp -*-

(defsystem detome
  :description "Detome game"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on (blacker)
  :components
  ((:file "package")
   (:file "utils")
   (:file "image-names")
   ;;(:file "dungeon")
   (:file "test"))   
  :serial t)