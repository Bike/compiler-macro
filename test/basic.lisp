(in-package #:sandalphon.compiler-macro-test)

(def-suite :sandalphon.compiler-macro)

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
