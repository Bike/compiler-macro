;;;; condition.lisp

;;;; various conditions, especially relating to giving up on expansion
;;;; (also with-expansion-declination etc.  this file may be more appropriately called "declination"...)

(in-package #:sandalphon.compiler-macro)

(define-condition optimization-note (style-warning) ()
  (:documentation "An optimization note signaled by a compiler expander of some sort.  Signaling this condition should indicate that some information (perhaps about how to improve efficiency by rearranging things to let the expander expand) should be presented to the programmer during compilation."))

;; this is in alexandria, but.
(define-condition simple-style-warning (simple-warning style-warning) ()
  (:documentation "A STYLE-WARNING that is also a SIMPLE-WARNING.  Initargs :FORMAT-CONTROL and :FORMAT-ARGS, and the readers SIMPLE-CONDITION-FORMAT-CONTROL and SIMPLE-CONDITION-FORMAT-ARGUMENTS, are available."))

(defun style-warn (datum &rest arguments)
  "Like WARN, but only signals STYLE-WARNINGs, and uses SIMPLE-STYLE-WARNING as the default class for condition designation."
  (warn (coerce-to-condition datum arguments 'simple-style-warning 'style-warning)))

(define-condition simple-optimization-note (optimization-note simple-style-warning) ()
  (:documentation "An OPTIMIZATION-NOTE that is also a SIMPLE-CONDITION.  Initargs :FORMAT-CONTROL and :FORMAT-ARGS, and the readers SIMPLE-CONDITION-FORMAT-CONTROL and SIMPLE-CONDITION-FORMAT-ARGUMENTS, are available."))

(defmacro check-type-eval (place typespec &optional type-string)
  "As CHECK-TYPE, but TYPESPEC is evaluated."
  ;; ripped from SBCL's check-type, and sans a single solitary quote for the semantic.  bleh.
  (let ((value (gensym "VALUE"))
	(_typespec (gensym "TYPESPEC"))
	(_type-string (gensym "TYPE-STRING")))
    `(do ((,value ,place ,place)
	  (,_typespec ,typespec)
	  (,_type-string ,type-string))
	 ((typep ,value ,_typespec))
       (setf ,place
	     (restart-case
		 (error 'simple-type-error
			:datum ,value
			:expected-type ,_typespec
			:format-control "The value of ~S is ~S, which is not ~:[of type ~S~;~:*~A~]."
			:format-arguments (list ',place ,value ,_type-string ,_typespec))
	       (store-value (value)
		 :report (lambda (stream) (format stream "Supply a new value for ~s." ',place))
		 value))))))

(defun coerce-to-condition (datum args default-type supertype)
  "This function implements the semantics of CL \"condition designators\".  It makes a condition, given a DATUM (which may be a symbol, format control, or condition), and ARGS (a list of arguments).  See CLHS 9.1.2.1 for more specifics.

DEFAULT-TYPE is the type of objects that should be constructed when DATUM is a format control.  SUPERTYPE is a type that should be a supertype of the types of all conditions returned by this function."
  (etypecase datum
    ;; just a symbol, not a class, says 9.1.2.1. why? who knows!
    ;; and of course (deftype foo (...args...) ... (find-class 'some-kind-of-condition))
    ;; (error '(foo ...) ...) is right out.
    (symbol
     (if (subtypep datum supertype)
	 (apply #'make-condition datum args)
	 (error "~s is not a subclass of ~s, and can't be used as one" datum supertype)))
    ;; functions are also format controls.
    ((or function string) (make-condition default-type :format-control datum :format-arguments args))
    (condition
     (check-type-eval datum supertype)
     (unless (null args)
       (cerror "Ignore the extra arguments."
	       "Passed a condition to ~s, but passed arguments ~s as well."
	       'coerce-to-condition args))
     datum)))

(defun decline-expansion (&optional (datum nil datum-p) &rest args)
  "This function should be called by compiler macroexpanders that reach a point such that they do not wish to expand.

If DATUM is provided, NOTE-OPTIMIZATION-FAILURE is called with DATUM and ARGS to inform the programmer of the situation.

Regardless of whether DATUM is provided, if called in the appropriate dynamic context, a non-local control transfer will immediately occur, halting expansion."
  (when datum-p
    (apply #'note-optimization-failure datum args))
  (throw 'decline nil))

(defmacro with-expansion-declination (&body body)
  "Within the dynamic extent of BODY, DECLINE-EXPANSION will immediately return NIL from the WITH-EXPANSION-DECLINATION form.

This is intended for use in \"advanced\" utilities."
  ;; and yet it's a oneliner.  not enough scarequotes in the world for that "advanced". 「«“"'advanced'"”»」.
  `(catch 'decline ,@body))

(defun abort-expansion (&optional (datum nil datum-p) &rest args)
  "Like DECLINE-EXPANSION, but immediately aborts all expansion of the form (e.g. including in other hints), forcing the form to be used as-is.  This should be used, for example, when enough analysis is done to see that a form will be an error at runtime."
  (when datum-p
    (apply #'note-optimization-failure datum args))
  (throw 'abort nil))

(defmacro with-expansion-abortion (&body body)
  "Within the dynamic extent of BODY, ABORT-EXPANSION will immediately return NIL from the WITH-EXPANSION-DECLINATION form.

This is intended for use in \"advanced\" utilities."
  ;; of course people could just make their own with note-optimization-failure and throw/block interleaving
  ;; (like i do with hints)
  ;; FIXME: This and its sister could probably use better names.
  ;; FIXME: And maybe some multiple value garbage to distinguish returning nil and aborting and blaaaaa
  `(catch 'abort ,@body))

(defun note-optimization-failure (datum &rest args)
  "DATUM and ARGS are treated as a condition designator (type OPTIMIZATION-NOTE, default SIMPLE-OPTIMIZATION-NOTE).  WARN is called with this designated condition.

This is intended to be used to display missed optimization opportunities and related information to a programmer during compilation."
  (warn (coerce-to-condition datum args 'simple-optimization-note 'optimization-note)))

;;; doesn't particularly belong...
(define-condition compiler-macro-redefinition-warning (style-warning)
  ((name :initarg :name :reader compiler-macro-redefinition-warning-name))
  (:documentation "A style warning signaled when a compiler macro is redefined incompatibly.
The redefined name is readable with COMPILER-MACRO-REDEFINITION-WARNING-NAME."))
