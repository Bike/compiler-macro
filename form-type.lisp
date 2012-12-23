;;;; form-type.lisp

;;;; basic interface to type information.
;;;; (type inferrence is in infer.lisp.)

(in-package #:sandalphon.compiler-macro)

(defun form-type (form &optional env)
  "Return a type such that (typep [form] [type] [env]) would return true, if [form] was evaluated in [env]."
  (let ((form (macroexpand form env)))
    (typecase form
      (symbol (variable-type form env))
      (cons
       (let ((inferrer (find-inferrer (first form))))
	 (if inferrer ; custom inference overrides ftypes completely.
	     (funcall inferrer form env)
	     (function-type-primary-value (function-type (first form) env)))))
      (t `(eql ,form)))))

(defun form-typep (form type &optional env)
  "Returns true if FORM would evaluate to a value of type TYPE in ENV, else false."
  (subtypep (form-type form env) type env))

(defmacro form-typecase (form-form env-form &body cases)
  "Executes the cdr of the first case such that FORM-FORM's type (derived through FORM-TYPE with ENV-FORM) is a subtype of the car of that case.  If no case matches, NIL is returned."
  (let ((_env (gensym "ENV"))
	;; form-form doesn't have to be gensymed, since case-body does that.
	(_subtypep (gensym "SUBTYPEP")))
    `(let ((,_env ,env-form))
       (flet ((,_subtypep (type1 type2)
		(subtypep type1 type2 ,_env)))
	 (declare (inline ,_subtypep) (dynamic-extent (function ,_subtypep)))
	 ,(case-body 'form-typecase `(form-type ,form-form ,_env) cases _subtypep nil)))))
