;;;; expt.lisp

;;; peval exponetiation on constant args

(in-package #:compiler-macro.test)

(defpackage #:common-lisp/expt
  (:nicknames #:cl/expt)
  (:use #:cl)
  (:shadow #:expt)
  (:export #:expt))

(defun cl/expt:expt (base power) (expt base power))

(define-compiler-hint expt (base power)
  (unless (constantp power) (decline-expansion))
  (let ((power (constant-form-value power)))
    (unless (and (integerp power)
                 (form-typep base '(not float)))
      (decline-expansion))
    (pingala base power)))

(defun pingala (base power)
  (if (zerop power)
      '1
      (let ((bindings nil)
            (name (gensym "PINGALA")))
        (loop (when (= 1 power)
                (return `(let* ((,name ,base)
                                ,@bindings)
                           ,name)))
           (multiple-value-bind (quo rem)
               (truncate power 2)
             (push `(,name ,(if (zerop rem)
                                `(* ,name ,name)
                                `(* ,base ,name ,name)))
                   bindings)
             (setf power quo))))))

(defun cl-user::test-expt (power methods
                           &optional
                           (repetitions 10000)
                           (optimize '((type (complex single-float) x) (optimize (speed 3) (safety 0) (space 0)))))
  (let ((functions (mapcar (lambda (method)
                             (let ((*error-output* (make-broadcast-stream)))
                               (format t "~&Compiling with ~s" method)
                               (time (compile nil `(lambda (x) (declare ,@optimize) ,(funcall method 'x power))))))
                           methods)))
    (mapc (lambda (method fn)
            (format t "~&Running with ~s" method)
            (time (loop repeat repetitions do (funcall fn (complex (random 0.5) (random 0.5))))))
          methods
          functions)
    functions))
