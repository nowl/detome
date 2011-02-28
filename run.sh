#!/bin/sh

sbcl --load "init.lisp" --eval "(detome:detome)" --eval "(quit)"
