(defpackage :blackfish
  (:nicknames #:bf)
  (:use #:cl 
		#:lispbuilder-sdl)
  (:shadow #:log)
  (:export #:mainloop
           #:get-tick-count
           #:init
           #:render
           #:update

           ;; log
           #:log
           #:set-log-level
           
           ;; message
           #:process-message
           #:add-message-listener
           #:remove-message-listener
           #:send-message
           #:message-sender
           #:message-receiver
           #:message-type
           #:message-payload

           ;; random
           #:random-choice
           #:random-weighted-choice

           ;; perlin
           #:perlin2d
           #:*perlin-seed*

           ;; parameters
           #:*screen-width*
           #:*screen-height*

           ;; mid-displace
           #:mid-displace

           ;; los
           #:line-of-sight

           ;; image
           #:get-image
           #:define-image
           #:clear-image-caches))