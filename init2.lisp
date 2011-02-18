(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/nowl/quicklisp/setup.lisp"))

(ql:quickload "lispbuilder-sdl")

(asdf:oos 'asdf:load-op 'detome)