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
   (:file "level"))
  :serial t)
