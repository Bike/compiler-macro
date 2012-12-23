;;;; miscellaneously useful code

(in-package #:sandalphon.compiler-macro)

(defun case-body (name keyform cases fun errorp)
  "Helper function for CASE-like macros."
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(mapcar (lambda (case) `((,fun ,key ',(first case)) ,@(rest case))) cases)
	     ;; TODO: make error suck less
	     ,@(when errorp (list `(t (error "~s fell through ~s expression" ,key ',name))))))))
