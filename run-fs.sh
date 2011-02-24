#!/bin/sh

sbcl --load "init.lisp" --eval "(detome:detome t)" --eval "(quit)"
