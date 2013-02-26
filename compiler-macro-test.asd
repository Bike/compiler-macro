(asdf:defsystem :compiler-macro-test
  :depends-on (:compiler-macro :fiveam)
  :version "0.1"
  :components ((:module "test"
			:components ((:file "package")
				     (:file "basic" :depends-on ("package")))))
  ;; cargo-culted from bordeaux-threads-test
  :in-order-to ((asdf:test-op (asdf:load-op compiler-macro-test)))
  :perform (asdf:test-op :after (op c) (funcall (find-symbol "RUN!" :fiveam) :sandalphon.compiler-macro)))
