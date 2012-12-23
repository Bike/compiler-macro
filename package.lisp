;;;; package.lisp

(defpackage #:sandalphon.compiler-macro-backend
  (:use #:cl)
  (:export #:variable-type #:function-type #:parse-macro #:parse-compiler-macro)
  (:export #:policy #:policy-quality)
  (:export #:typexpand #:typexpand-1))

(defpackage #:sandalphon.compiler-macro
  (:use #:cl #:sandalphon.compiler-macro-backend)
  (:export #:policy #:policy-quality) ; reexport from backend
  (:export #:compiler-macro-redefinition-warning
	   #:compiler-macro-redefinition-warning-name
	   #:note-optimization-failure
	   #:decline-expansion #:abort-expansion
	   #:with-expansion-declination #:with-expansion-abortion
	   #:optimization-note #:simple-optimization-note) ; condition
  (:export #:form-type #:form-typep
	   #:form-typecase) ; form-type
  (:export #:find-inferrer #:define-inferrer) ; infer
  (:export #:define-compiler-hinter
	   #:define-compiler-hint #:compiler-hint) ; hint
  (:export #:compiler-macroexpand #:compiler-macroexpand-1) ; expander
  (:export #:function-type-return-type #:values-type-primary
	   #:function-type-primary-value #:array-type-element-type
	   #:array-type-dimensions
	   #:union-type-types #:intersection-type-types #:negation-type-type
	   #:numeric-type-high #:numeric-type-low
	   #:complex-type-element-type
	   #:eql-type-object #:member-type-members
	   #:satisfies-type-function
	   #:cons-type-car-type #:cons-type-cdr-type) ; type-util
  (:export #:kind-of #:kindp #:kindcase #:ekindcase) ; kinds
  (:export #:cons-type #:array-type #:sequence-type ; kind specifiers (...)
	   #:complex-type #:numeric-type #:condition-type
	   #:intersection-type #:union-type #:member-type
	   #:eql-type #:satisfies-type #:negation-type)
  )
