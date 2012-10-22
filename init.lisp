(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/nowl/quicklisp/setup.lisp"))

(push #p"/home/nowl/dev/detome/" asdf:*central-registry*)
(push #p"/home/nowl/dev/detome/blacker/" asdf:*central-registry*)

;;(asdf:oos 'asdf:load-op 'blacker)

(asdf:oos 'asdf:load-op 'detome)
