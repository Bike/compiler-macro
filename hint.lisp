;;;; hint.lisp
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

;;;; a basic mechanism for multiple compiler macros on the same symbol.
;;;; see condition.lisp for how decline-expansion, with-expansion-declination etc. work.

;;;; TODO: better names

(in-package #:sandalphon.compiler-macro)

(defvar *compiler-hints* (make-hash-table)
  "A hash table from function names to alists of compiler macro hints.")

(defun compiler-hinter-lambda (name)
  "Return a compiler macroexpander closure that calls all the hints (in *COMPILER-HINTS*) for the
given NAME, and returns the first successful expansion (or the form it's provided with)."
  ;; sure is arrow in here
  (lambda (form env)
    (block done
      (with-expansion-abortion
        (dolist (entry (gethash name *compiler-hints*))
          (with-expansion-declination
            (let ((new (funcall (cdr entry) form env)))
              (unless (eql form new) ; handle old-style compiler macro declination semantics
                (return-from done new))))))
      ;; we aborted or didn't find an expansion, so
      form)))

(defmacro define-compiler-hinter (name lambda-list &body options)
  "Define NAME to have a hinter expansion as its compiler macro.  See DEFINE-COMPILER-HINT's
documentation.

Supported options: :documentation"
  (declare (ignore lambda-list))
  (let* ((doc-p (assoc :documentation options))
         (doc (second doc-p)))
    ;; more options later, e.g. "method combinations"
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (compiler-macro-function ',name)
         (warn 'compiler-macro-redefinition-warning
               :name ',name))
       (setf (gethash ',name *compiler-hints*) nil)
       (setf (compiler-macro-function ',name)
             (compiler-hinter-lambda ',name))
       ,@(when doc-p
               (list `(setf (documentation ',name 'compiler-macro) ',doc)))
       ',name)))

(defmacro define-compiler-hint (name lambda-list qual &body body &environment env)
  "Define a compiler hint for NAME.

LAMBDA-LIST is a compiler macro lambda list, that is a macro lambda list, and with BODY will be used
to form a hint expander function.

QUAL is an arbitrary object, which is compared (with CL:EQUAL) to establish uniqueness of the hint,
for redefinition, and retrieval with COMPILER-HINT.

Hint functions have an implicit block with the usual name, can have declarations and docstrings, etc.

Hint functions can call DECLINE-EXPANSION in order to decline to expand immediately. This is intended
as a replacement for the old-style \"return the original form\" protocol, though that is also
supported."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; ew, double hash lookup
     (let ((existing? (compiler-hint ',name ',qual)))
       (when existing?
         (warn 'compiler-macro-redefinition-warning :name ',name)))
     ;; TODO: set doc to qual? (or use the documentation from the function itself)
     (setf (compiler-hint ',name ',qual)
           ,(parse-compiler-macro name lambda-list body env))))

(defun compiler-hint (name qual)
  "Retrieve the hint function for NAME, identified by QUAL compared via CL:EQUAL.

A hint function is a function of two arguments, a form and an environment, and which returns a form
with the same semantics as FORM but (hopefully) more efficient.  A hint function should be prepared to
receive a form beginning with its name, or a form beginning with CL:FUNCALL."
  (cdr (assoc qual (gethash name *compiler-hints*))))

(defun (setf compiler-hint) (new-value name qual)
  "Set the hint function for NAME, identified by QUAL compared via CL:EQUAL.

A hint function is a function of two arguments, a form and an environment, and which returns a form
with the same semantics as FORM but (hopefully) more efficient, or otherwise changed."
  (check-type new-value function)
  ;; (setf (alexandria:assoc-value (gethash name *compiler-hints*) name :test #'equal) new-value)
  (let ((alist (gethash name *compiler-hints*)))
    (if alist
        (let ((assoc (assoc qual alist :test #'equal)))
          (if assoc
              (setf (cdr assoc) new-value)
              (push (cons qual new-value) (gethash name *compiler-hints*))))
        (setf (gethash name *compiler-hints*)
              (list (cons qual new-value)))))
  new-value)
