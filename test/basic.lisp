(in-package #:sandalphon.compiler-macro-test)

(def-suite :sandalphon.compiler-macro)

(defun make-env-form (var wrappers body)
  (let* ((macro-name (gensym))
	 (call `(,macro-name)))
    (mapc (lambda (wrapper)
	    (ecase (first wrapper)
	      ((declare) (setf call `(locally ,wrapper ,call)))
	      ((let) (setf call `(let (,@(rest wrapper))
				   (declare (ignorable ,@(mapcar (lambda (x) (if (consp x) (first x) x)) (rest wrapper))))
				   ,call)))
	      ((macrolet) (setf call `(macrolet (,@(rest wrapper)) ,call)))
	      ((flet) (setf call `(flet (,@(rest wrapper))
				    (declare (ignorable ,@(mapcar (lambda (x)
								    `(function ,(if (consp x) (first x) x)))
								  (rest wrapper))))
				    ,call)))))
	  (reverse wrappers))
    `(macrolet ((,macro-name (&environment ,var) ,@body nil))
       ,call)))

(defmacro in-environment (env-var (&rest wrappers) &body body)
  `(eval ',(make-env-form env-var wrappers body)))

(in-suite :sandalphon.compiler-macro)

(defun memq (item list)
  (member item list :test #'eq))

(define-compiler-macro memq (&whole whole item list &environment env)
  (if (constantp list env)
      (let ((list (eval list)))
	;; this expansion is wrong - item can be evaluated multiple times.
	;; but this is just for testing.
	`(or ,@(mapcar (lambda (com) `(eq ,item ',com)) list)))
      whole))

(defun memb (item list &key (test #'eql))
  (member item list :test test))

(test compiler-macroexpand-1
  (setf (compiler-macro-function 'memb) nil)
  (define-compiler-macro memb (&whole whole item list &key test)
    (if (equal test '(function eq))
	`(memq ,item ,list)
	whole))
  (is (equal '(memq x (foo))
	     (compiler-macroexpand-1 '(memb x (foo) :test #'eq))))
  #+ccl
  (skip "CCL's compiler macros don't deal with ~s forms as they should." 'funcall)
  #-ccl
  (is (equal '(memq x (foo))
	     (compiler-macroexpand-1 '(funcall #'memb x (foo) :test #'eq))))
  (let ((form '(memb x (foo)))
	#-ccl
	(funcall-form '(funcall #'memb x (foo))))
    (is (eql form (compiler-macroexpand-1 form)))
    #+ccl
    (skip "CCL's compiler macros don't deal with ~s forms as they should." 'funcall)
    #-ccl
    (is (eql funcall-form (compiler-macroexpand-1 funcall-form)))))

(test compiler-macroexpand
  (is (equal '(memq x (foo))
	     (compiler-macroexpand '(memb x (foo) :test #'eq))))
  (is (equal (compiler-macroexpand '(or (eq x 'a) (eq x 'b))) ; in case the implementation has a cmf on OR for some reason
	     (compiler-macroexpand '(memb x '(a b) :test #'eq)))))

(test compiler-macroexpand-environment
  (in-environment env
      ((declare (notinline memb)))
    (let ((form '(memb x (foo) :test #'eq)))
      (is (eql form (compiler-macroexpand form env))
	  "~s does not properly respect ~s declarations"
	  'compiler-macroexpand
	  'notinline)))
  (in-environment env
      ((flet (memb (&rest args) 11)))
    (let ((form '(memb x (foo) :test #'eq)))
      (is (eql form (compiler-macroexpand form env))
	  "~s does not properly respect lexical shadowing of functions"
	  'compiler-macroexpand))))

(test compiler-macroexpand-hook
  (let* ((*counter* 0)
	 (*macroexpand-hook*
	  (compile nil
		   '(lambda (expander form env)
		      (declare (special *counter*))
		      (incf *counter*)
		      (funcall expander form env)))))
    (declare (special *counter*))
    (compiler-macroexpand '(memb x '(a b) :test #'eq))
    (is-true (> *counter* 0)
	     "~s does not respect ~s" 'compiler-macroexpand '*macroexpand-hook*)))

(test conditions
  ;; mostly running coerce-to-condition through its paces
  (signals simple-style-warning (style-warn "Not checking ~s and ~s." :format-control :format-args))
  (signals style-warning (style-warn (make-condition 'style-warning)))
  (signals style-warning (style-warn 'style-warning))
  (signals error (style-warn 'simple-warning))
  (signals simple-style-warning (style-warn (formatter "Confusing the hell ~
out of people who don't realize that functions count as format controls.")))
  (signals simple-optimization-note (note-optimization-failure "Do tests even have to worry about efficiency?"))
  (signals optimization-note (note-optimization-failure (make-condition 'optimization-note)))
  (signals type-error (note-optimization-failure (make-condition 'simple-warning))))

(test types
  ;; fuck, man.
  ;; a lot of this should be rexamined - using type equality or set equality predicates, that sort of thing.
  (is (equal '(values integer &optional)
	     (function-type-return-type '(function nil (values integer &optional)))))
  (is (eql 'integer (values-type-primary 'integer)))
  (is (eql 'null (values-type-primary '(values))))
  (is (eql 'integer (values-type-primary '(values integer))))
  (is (eql 'integer (values-type-primary '(values &optional integer))))
  (is (eql 'integer (function-type-primary-value '(function nil (values integer &optional)))))
  (is (eql 'integer (array-type-element-type '(array integer))))
  (is-true (subtypep (array-type-element-type 'string) 'character))
  (is (eql '* (array-type-dimensions '(simple-array * *))))
  (is (eql '* (array-type-dimensions 'vector)))
  (is (equal '(4 *) (array-type-dimensions '(simple-array nil (4 *)))))
  (is (equal '(integer) (union-type-types '(or integer))))
  (is (equal '(integer) (intersection-type-types '(and integer))))
  (is (eql 'integer (negation-type-type '(not integer))))
  (is (eql '* (numeric-type-low 'integer)))
  (signals error (numeric-type-low '(complex integer)))
  (is (= most-negative-fixnum (numeric-type-low 'fixnum)))
  (is (= (- (ash 1 (1- 7))) (numeric-type-low '(signed-byte 7))))
  (is (eql '* (numeric-type-low '(signed-byte))))
  (is (= 7 (numeric-type-low '(integer 7))))
  (is (zerop (numeric-type-low '(mod 12))))
  (is (equal '(0.7) (numeric-type-low '(short-float (0.7) 4.7))))
  (is (= most-positive-fixnum (numeric-type-high 'fixnum)))
  (is (= (1- (ash 1 12)) (numeric-type-high '(unsigned-byte 12))))
  (is (equal '(long-float (6.6)) (complex-type-element-type '(complex (long-float (6.6))))))
  (is (eql #\a (eql-type-object '(eql #\a))))
  (is (equal '(#\b) (member-type-members '(member #\b))))
  (is (eql 'plusp (satisfies-type-function '(satisfies plusp))))
  (is (equal '(cons integer) (cons-type-car-type '(cons (cons integer) float))))
  (is (eql 'float (cons-type-cdr-type '(cons (cons integer) float)))))

(test kind-of
  (is (eq 'cons-type (kind-of '(cons * integer))))
  (is (eq 'array-type (kind-of 'simple-array)))
  (is (eq 'bottom-type (kind-of 'nil)))
  (is (eq 'condition-type (kind-of 'division-by-zero)))
  (is (eq 'complex-type (kind-of '(complex double-float))))
  (is (eq 'sequence-type (kind-of 'sequence)))
  (is (eq 'numeric-type (kind-of '(mod 23))))
  (is (eq 'eql-type (kind-of '(eql #\a))))
  (is (eq 'union-type (kind-of '(or (eql lambda) integer))))
  (is (eq 'member-type (kind-of '(member not or and))))
  (is (eq 'class (kind-of 'standard-class)))
  (is (eq (class-of (find-class 'structure-class)) (kind-of (find-class 'structure-class)))))

(test kindp
  (is-true (kindp '(and fixnum (satisfies plusp)) 'intersection-type))
  (is-true (kindp '(or integer string) 'union-type))
  (is-true (kindp '(member 4 7) 'member-type))
  (is-true (kindp '(eql #\b) 'eql-type))
  (is-false (kindp '(eql #\b) 'member-type))
  (is-true (kindp '(satisfies plusp) 'satisfies-type))
  (is-true (kindp '(not (satisfies plusp)) 'negation-type))
  (is-true (kindp '(cons integer) 'cons-type))
  (is-true (kindp '(string 5) 'array-type))
  (is-true (kindp '(cons 4 5) 'sequence-type))
  (is-true (kindp '(complex (satisfies minusp)) 'complex-type))
  (is-true (kindp '(complex (satisfies minusp)) 'numeric-type))
  (is-true (kindp (find-class 'integer) 'class))
  (is-false (kindp (find-class 'integer) 'numeric-type))
  (is-true (kindp '(simple-string 5) '(and array-type (subtype string)))))

#+sandalphon.cltl2
(progn

  (deftype foo (&optional dim) `(array integer ,dim))
  (deftype bar () '(foo (7)))

  (test typexpand-1
    (is (equal '(array integer *) (typexpand-1 '(foo))))
    (is (equal '(array integer (4 * 7)) (typexpand-1 '(foo (4 * 7)))))
    (is (equal '(foo (7)) (typexpand-1 '(bar)))))

  (test typexpand
    (is (equal '(array integer (7)) (typexpand '(bar)))))

  (test variable-type
    (in-environment env
	((let (x 'x))
	 (declare (type symbol x)))
      (is (eq 'symbol (variable-type 'x env)))))

  (test function-type
    (in-environment env
	((flet (x () 'x))
	 (declare (ftype (function nil (values symbol &optional)) x)))
      (is (equal '(function nil (values symbol &optional))
		 (function-type 'x env)))))

  (test policy
    (in-environment env
	((declare (optimize (debug 3) (speed 2) (compilation-speed 1) (space 0))))
      (is-true (policy (> debug compilation-speed space) env))
      (is (= 2 (policy-quality 'speed env)))))) ; #+sandalphon.cltl2

(defmacro compiler-lambda (name lambda-list &body body &environment env)
  (parse-compiler-macro name lambda-list body env))

(defmacro expands-to (expander expansion expandee env &rest reason-args)
  `(is (equal ',expansion (funcall *macroexpand-hook* ,expander ',expandee ,env)) ,@reason-args))

(test parse-compiler-macro
  (expands-to (compiler-lambda foo () '(bar)) (bar) (foo) nil)
  (let ((fn (compiler-lambda foo (&whole whole ((a) &rest b) &optional (why "hey"))
	      (cond ((eq a 'none)
		     `(bar ,a why '(,@b ,why) a))
		    ((eq a 'return)
		     (return-from foo whole))))))
    (is (functionp fn))
    (expands-to fn
		(bar none why '(67 68 "hey") a)
		(foo ((none) 67 68))
		nil
		"basic failure in lambda list parsing")
    (expands-to fn
		(foo ((return)))
		(foo ((return)))
		nil
		"parsed compiler macro does not establish block")
    (expands-to fn
		(bar none why '('x) a)
		(funcall #'foo ((none)) 'x)
		nil
		"parsed compiler macro does not deal with funcall correctly"))
  ;; haha oh man
  (in-environment env
      ((macrolet (foo () 'baz)))
    (expands-to (compiler-lambda foo (bar &environment env) (macroexpand bar env))
		baz
		(foo (foo))
		env
		"&environment parsing")
    (expands-to (compiler-lambda foo (&environment env bar) (macroexpand bar env))
		baz
		(foo (foo))
		env
		"&environment parsing in a different position")
    'nil))

(test form-type
  (is (eq 'fixnum (form-type '(the fixnum (foo)))))
  #+sbcl (is (eq 'character (form-type '(sb-ext:truly-the character (bar)))))
  (is (equal '(eql #\a) (form-type #\a)))
  (is (eq 'nil (form-type '(return (+ 2 2)))))
  (is (eq 'nil (form-type '(return-from handler-start))))
  (is (eq 'nil (form-type '(go :start))))
  (is (eq 'nil (form-type '(throw 'done 7)))))

(test form-typep
  (is-true (form-typep '(the (values character &optional list) #\a) 'character)))

(test form-typecase
  (form-typecase '(the integer (foo))
      nil
    (number (pass "~s basics work" 'form-typecase))
    (t (fail "~s basics don't work" 'form-typecase))))

(defun map1 (result-type function sequence)
  (map result-type function sequence))

(define-inferrer map1 (result-type function sequence &environment env)
  (declare (ignore function sequence))
  (if (constantp result-type env)
      (eval result-type)
      'sequence))

(defun id (obj) obj)
(define-inferrer id (obj &environment env)
  (form-type obj env))

(test customized-inference
  (is (eq 'sequence (form-type '(map1 seq-type #'1+ #(4 5 6)))))
  (is (equal '(array integer) (form-type '(map1 '(array integer) foo bar))))
  (is (equal '(simple-string 4) (form-type '(the (simple-string 4) (map1 'string #'name-char chars)))))
  (is (equal '(eql #\a) (form-type '(id #\a)))))



(test hints
  (define-compiler-hinter memb (item list &key test))

  (define-compiler-hint memb (item list &key test &environment env)
      "everything"
    (if (and (constantp item env) (constantp list env) (constantp test env))
	`',(memb (eval item) (eval list) :test (eval test))
	(decline-expansion)))
  
  (define-compiler-hint memb (item list &key test)
      "test"
    (cond ((equal test '#'eq)
	   `(memq ,item ,list))
	  (t (decline-expansion "constant test is not ~s" '#'eq))))
  
  (define-compiler-hint memb (item list &key test &environment env)
      "unfold"
    (if (and (constantp list env) (not (constantp item env)))
	`(let ((item ,item)
	       (test ,test))
	   (or ,@(mapcar (lambda (elt) `(funcall test item ',elt)) (eval list))))
	(decline-expansion)))

  (macrolet ((expands-to (expansion expandee)
	       `(is (tree-equal ',expansion (compiler-macroexpand-1 ',expandee)))))
    (expands-to '(4) (memb 4 '(7 8 4) :test '=))
    (expands-to (memq bar (foo)) (memb bar (foo) :test #'eq))
    (expands-to (let ((item (foo))
		      (test 'equal))
		  (or (funcall test item '(bar))
		      (funcall test
 item '(baz))))
		(memb (foo) '((bar) (baz)) :test 'equal)))
  (signals simple-optimization-note (compiler-macroexpand-1 '(memb foo bar :test #'eql)))
  (is-false (nth-value 1 (compiler-macroexpand-1 '(memb foo bar :test baz)))))
