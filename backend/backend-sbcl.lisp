;;;; backend-sbcl.lisp

;;;; implementation-independent documentation can be found in backend-doc

(in-package #:sandalphon.compiler-macro-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; what a pain
  (shadowing-import '(sb-ext:typexpand-1 sb-ext:typexpand
		      sb-cltl2:function-information sb-cltl2:variable-information
		      sb-cltl2:declaration-information sb-cltl2:parse-macro)
	  '#:sandalphon.compiler-macro-backend)
  (export '(sb-ext:typexpand-1 sb-ext:typexpand
	    sb-cltl2:function-information sb-cltl2:variable-information
	    sb-cltl2:declaration-information sb-cltl2:parse-macro)
	  '#:sandalphon.compiler-macro-backend))

;;; implementations implementing the CLtL2 non-standard have this easy.

(defun variable-type (name &optional env)
  (or (cdr (assoc 'type (nth-value 2 (variable-information name env))))
      't))

(defun function-type (name &optional env)
  (or (cdr (assoc 'ftype (nth-value 2 (function-information name env))))
      '(function * *)))

(defun policy-quality (quality &optional env)
  (or (cdr (assoc quality (declaration-information 'optimize env)))
      (error "Unknown policy quality ~s" quality)))

(defmacro policy (expr &optional env)
  ;; conveniently, declaration-information 'optimize is specified to
  ;;   always return an alist with all optimization qualities.
  (let ((qualities (mapcar #'car (declaration-information 'optimize)))
	(optvar (gensym "POLICY")))
    `(let ((,optvar (declaration-information 'optimize ,env)))
       ;; sbcl, at least, has an alist of lists rather than alist of conses, hence SECOND.  weird.
       (symbol-macrolet (,@(mapcar (lambda (quality) `(,quality (second (assoc ',quality ,optvar)))) qualities))
	 ;; CLHS 11.1.2.1.2.1 (ref because wow obscure) explicitly allows standard symbols that aren't variables
	 ;;   to be symbol-macrolut.
	 ;; This may not be true of implementation-specific packages.  (it would be nice, but) (sbcl is fine, woo)
	 ,expr))))

;;; and now the other bits...

(defun parse-compiler-macro (name lambda-list body &optional env)
  ;; largely copied from sbcl's define-compiler-macro, unsurprisingly.
  (declare (ignore env)) ; env is just for evenness with parse-macro
  ;; variables for the expansion
  (let ((whole-var (gensym "WHOLE"))
	(env-var (gensym "ENV")))
    (multiple-value-bind (body local-decls doc)
	(sb-kernel:parse-defmacro lambda-list whole-var body name 'define-compiler-macro
				  ;; the d-c-m context tells sbcl to build the body to handle FUNCALL forms correctly.
				  ;; at least, "correctly" if you don't want a compiler macro on CL:FUNCALL
				  ;; (which is technically undefined anyway, so there's that)
				  :environment env-var)
      (declare (ignore doc)) ; welp.
      `(lambda (,whole-var ,env-var)
	 ,@local-decls
	 ,body))))

;;; alternate sbcl-specific definitions, probably less stable than the cltl2 half-standard

#+(or)
(defmacro policy (expr &optional env)
  (sb-c:policy env expr))

#+(or)
(defun policy-quality (quality &optional env)
  (let ((policy (sb-c::%coerce-to-policy env)))
    ;; %coerce instead of just ::lexenv-policy in case of a nil argument.
    ;; though, maybe it would be better to make+use a null lexenv in the NIL case, since NIL is coerced to a lack of policy.
    ;; (i.e., it's unaffected by toplevel declaims, etc)
    (cond ((member quality sb-c::*policy-qualities*)
	   (sb-c::policy-quality policy quality))
	  ((member quality sb-c::*policy-dependent-qualities*)
	   (let ((info (cdr (assoc quality sb-c::*policy-dependent-qualities*))))
	     (funcall (sb-c::policy-dependent-quality-getter info) policy)))
	  (t (error "Unknown policy quality ~s" quality)))))
