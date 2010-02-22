(in-package #:detome)

(define-monster-type
	"rat"
	"player-front"
	1
  #'(lambda ()
	  (1+ (random 5)))
  #'(lambda ()
	  (1+ (random 5)))
  #'(lambda ()
	  (1+ (random 5)))
  #'(lambda ()
	  (1+ (random 5)))
  #'(lambda (obj)
	  nil))

(define-monster-type
	"giant rat"
	"player-front"
	2
  #'(lambda ()
	  (1+ (random 8)))
  #'(lambda ()
	  (1+ (random 8)))
  #'(lambda ()
	  (1+ (random 8)))
  #'(lambda ()
	  (1+ (random 8)))
  #'(lambda (obj)
	  nil))

  
  