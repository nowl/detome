(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/nowl/quicklisp/setup.lisp"))

(asdf:oos 'asdf:load-op 'black-engine)
(asdf:oos 'asdf:load-op 'detome)