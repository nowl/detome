(defpackage :detome
  (:use #:cl 
		#:black)
  (:shadowing-import-from #:black 
                          #:log
                          #:remove)
  (:export #:detome))