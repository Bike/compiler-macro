;;;; compiler-macro.asd

(asdf:defsystem #:compiler-macro
  :description "Utilities for writing compiler macros."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.1"
  :depends-on (#+sbcl #:sb-cltl2)
  :components ((:file "package")
	       (:module "backend"
			:depends-on ("package")
			:serial t
			:components (;; indentation is hard
				     #+sbcl (:file "backend-sbcl")
				     #+ccl (:file "backend-ccl")
				     #-(or sbcl ccl) (:file "backend-default")
				     (:file "backend-doc")))
	       (:file "condition" :depends-on ("package"))
	       (:file "type-util" :depends-on ("package"))
	       (:file "misc" :depends-on ("package"))
	       (:file "kinds" :depends-on ("misc" "package"))
	       (:file "infer" :depends-on ("condition" ; for STYLE-WARN
					   "backend" "type-util" "package"))
	       (:file "form-type" :depends-on ("misc" "backend" "infer" "type-util" "package"))
	       (:file "hint" :depends-on ("backend" "condition" "package"))
	       (:file "expander" :depends-on ("package"))))
