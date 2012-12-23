;;;; backend-cltl2.lisp

;;;; implementations implementing the CLtL2 non-standard have this easy.

(defpackage #:compiler-macro.backend
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-cltl2
   #+ccl #:ccl
   #:function-information #:variable-information #:parse-macro))

(in-package #:compiler-macro.backend)

(defun variable-type (name env)
  (or (cdr (assoc 'type (nth-value 2 (variable-information name env))))
      't))

(defun function-type (name env)
  (or (cdr (assoc 'ftype (nth-value 2 (function-information (first form) env))))
      '(function * *)))
