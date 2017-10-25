;;;; infer.lisp
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

;;;; basic type inference

(in-package #:sandalphon.compiler-macro)

;;; type derivation could be fancier, but there are three issues with that:
;;; 1) the underlying compiler might do it anyway (sbcl especially) but not necessarily at compiler macro expansion time
;;;    (c.f. sb-c:deftransform), and why duplicate that?
;;; 2) type inference in lisp is hard, thanks to side effects and strict evaluation ordering.  we'd have to establish
;;;    dynamic environments or something
;;; 3) for most real applications THE and DECLARE TYPE are probably sufficient if you just want to trigger some macros

;;; it would be nice if there was at least a "pure function" declaration to allow slightly less minimal inference,
;;; but oh well.

;;; actually we can spice it up a /little/, at least.

;;; TODO: Fix spurious redefinitions caused by recompiling an inferrer

(defvar *inferrers* (make-hash-table))

(defun find-inferrer (name)
  "Return the type inference function for NAME."
  (gethash name *inferrers*))
(defun (setf find-inferrer) (new-value name)
  "Set the type inference function for NAME.  The function should be one suitable as a macroexpander, except that it should return a type specifier.  See DEFINE-INFERRER for more information."
  (check-type new-value function)
  (setf (gethash name *inferrers*) new-value))

(defmacro define-inferrer (name lambda-list &body body &environment env)
  "Define a type inference function for NAME.  In FORM-TYPE, cons forms beginning with NAME will be passed to the function defined by LAMBDA-LIST (which is a macro lambda list) and BODY; the type specifier returned will be used as the inferred type.

E.g.
 (define-inferrer the (value-type form)
  (declare (ignore form))
  (when (eql '* value-type)
    (warn \"~s illegal in THE\" '*))
  (value-type-primary value-type))

Note that in Lisp complex inference is inhibited by side effects and the mandated evaluation order.  Be conservative.

Defined inferrers can be accessed with FIND-INFERRER."
  ;; TODO: docstrings?
  `(progn
     (when (find-inferrer ',name)
       (style-warn "Redefining inferrer for ~s" ',name))
     (setf (find-inferrer ',name)
           ,(parse-macro name lambda-list body env))))

(define-inferrer the (&whole the value-type form)
  (declare (ignore form))
  (when (eql '* value-type)
    (warn "~s illegal in ~s" '* (first the)))
  (values-type-primary value-type))

(define-inferrer function (name &environment env)
  (cond ((or (symbolp name) (and (listp name) (eq (first name) 'setf)))
         ;; name
         (function-type name env))
        ((and (listp name) (eq (first name) 'lambda))
         '(function * *))
        (t
         ;; i dunno
         (warn "invalid ~s form ~s" 'function `(function ,name)))))

(define-inferrer quote (thing)
  `(eql ,thing))

(define-inferrer return-from (name &optional value)
  (declare (ignore name value))
  'nil)

(define-inferrer go (tag)
  (declare (ignore tag))
  'nil)

(define-inferrer throw (tag result)
  (declare (ignore tag result))
  'nil)

#+sbcl (setf (find-inferrer 'sb-ext:truly-the) (find-inferrer 'the))
