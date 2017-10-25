;;;; miscellaneously useful code
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

(in-package #:sandalphon.compiler-macro)

(defun case-body (name keyform cases fun errorp)
  "Helper function for CASE-like macros."
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(mapcar (lambda (case) `((,fun ,key ',(first case)) ,@(rest case))) cases)
	     ;; TODO: make error suck less
	     ,@(when errorp (list `(t (error "~s fell through ~s expression" ,key ',name))))))))
