;;;; package.lisp
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

(defpackage #:sandalphon.compiler-macro
  (:use #:cl #:introspect-environment)
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
	   #:bottom-type #:subtype
	   #:complex-type #:numeric-type #:condition-type
	   #:intersection-type #:union-type #:member-type
	   #:eql-type #:satisfies-type #:negation-type)
  )
