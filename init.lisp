(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (dir (directory "C:\\Documents and Settings\\mjp\\.sbcl\\systems\\*"))
	(push dir asdf:*central-registry*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'cffi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push "c:\\Documents and Settings\\mjp\\Desktop\\incoming\\sdl_test\\" cffi:*foreign-library-directories*)
  (asdf:oos 'asdf:load-op 'lispbuilder-sdl)
  (asdf:oos 'asdf:load-op 'lispbuilder-sdl-image))

(asdf:oos 'asdf:load-op 'detome)

