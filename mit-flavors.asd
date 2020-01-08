





(asdf:defsystem :flavors
    :depends-on (:cffi :alexandria ::bordeaux-threads :closer-mop
		       :zeta-lisp-compatability)
    :serial t
    :components
    ((:module mit-flavors
	      :depends-on (:package :load-clim)
	      :serial t
	      :components
	      ((:file package
	       (:file demo-driver)))))
