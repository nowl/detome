(load "~/quicklisp/setup.lisp")

(push #p"~/dev/detome/blacker/" asdf:*central-registry*)

(require 'blacker)

(swank:set-package "blacker")
