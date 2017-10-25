#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

(asdf:defsystem :compiler-macro-test
  :depends-on (:compiler-macro :fiveam)
  :version "0.1"
  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "basic" :depends-on ("package")))))
  ;; cargo-culted from bordeaux-threads-test
  :in-order-to ((asdf:test-op (asdf:load-op compiler-macro-test)))
  :perform (asdf:test-op :after (op c) (funcall (find-symbol "RUN!" :fiveam) :sandalphon.compiler-macro)))
