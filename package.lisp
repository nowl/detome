(defpackage :detome
  (:use #:cl 
		#:black)
  (:shadowing-import-from #:black #:log)
  (:export #:run-detome))