;;;; kinds.lisp
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

;;;; types of types, and *KINDCASE (for an example use of that, see examples/tabasco-sort)

(in-package #:sandalphon.compiler-macro)

(defparameter *kind-types*
  '((bottom-type . nil) ; has to be first, since (subtypep nil anything) => T
    (cons-type . cons) (array-type . array) (sequence-type . sequence)
    (complex-type . complex) (numeric-type . number)
    ;; handled specially since no relation between CONDITION and CLASS is specified, etc
    (condition-type . condition)
    (class . class))
  "For some kinds, (kindp foo KIND) is roughly (subtypep foo TYPE).  This alist maps KINDs to TYPEs
for such cases.")

(defun kind-of (type)
  "Return a kind designator appropriate for TYPE.  See KINDP for an explanation of kinds."
  (flet ((shared-name (name)
           (mapc (lambda (cons)
                   (when (subtypep name (cdr cons))
                     (return-from shared-name (car cons))))
                 *kind-types*)
           (error "Nope")))
    (typecase type
      (cons (case (first type)
              ;; first pick off the ones that aren't legal as atomic specifiers.
              ((and) 'intersection-type)
              ((or) 'union-type)
              ((member) 'member-type)
              ((eql) 'eql-type) ; member-type?
              ((satisfies) 'satisfies-type)
              ((not) 'negation-type)
              ;; use the whole type instead of just the car for e.g. (mod *)
              (t (shared-name type))))
      (symbol (or (shared-name type)
                  (let ((class (find-class type nil)))
                    ;; find-class works on defclass-defined and defstruct-defined classes.
                    ;; not necessarily conditions, which is why we handle it in shared-name.
                    (if class
                        (class-of class)
                        (error "bluh bluh")))))
      (class (class-of type)))))

(defun kindp (type kind &optional environment)
  "Like TYPEP, but for types themselves - TYPE is a type, and KIND is a kind designator.  \"kinds\"
are like types of types; for example, we might say that (array * (3)) is of kind ARRAY-TYPE.
Reasoning about types in this fashion is frequently useful in reasoning about programs.

Possible kind designators are:

INTERSECTION-TYPE: Kind of (AND ...)
UNION-TYPE: Kind of (OR ...)
MEMBER-TYPE, EQL-TYPE, SATISFIES-TYPE are as is obvious from their names.
NEGATION-TYPE: Kind of (NOT ...)

CONS-TYPE, ARRAY-TYPE, SEQUENCE-TYPE, COMPLEX-TYPE, NUMERIC-TYPE are all as obvious, covering both
atomic and compound (if this is applicable) types of their names.  CONDITION-TYPE is similar, and
included because the CL standard doesn't prescribe any particular representation for the class of
CONDITION.

BOTTOM-TYPE is used exclusively for NIL, the type of no objects.

Other kinds are already integrated with CLOS.  E.g., STRUCTURE-CLASS (the name, or the class itself)
is the kind of structure types, and CLASS is the kind of classes.

Compound kind designators are available.  EQL, OR, AND, and NOT are analogous to the same operators of
types.  SUBTYPE can be used to designate the kind of all subtypes of a given type; that is, (kindp [t1]
'(subtype [t2])) is the same as (subtypep [t1] [t2])."
  (flet ((subtypecheck (type)
           (let ((assoc (assoc kind *kind-types*)))
             (if assoc
                 (subtypep type (cdr assoc) environment)
                 (if (member kind '(intersection-type union-type
                                    member-type eql-type
                                    satisfies-type negation-type))
                     nil
                     (error "unknown kind ~s" kind))))))
    (let ((type (typexpand type environment)))
      (cond ((consp kind)
             (ecase (first kind)
               ((eql) (eql (second kind) type))
               ((subtype) (subtypep type (second kind) environment))
               ((or)
                ;; force a boolean
                (if (some (lambda (kind) (kindp type kind environment)) (rest kind))
                    t
                    nil))
               ((and) (every (lambda (kind) (kindp type kind environment)) (rest kind)))
               ((not) (not (kindp type (second kind) environment)))))
            ((consp type)
             (case (first type)
               ;; types to pick off specifically
               ((and) (eql kind 'intersection-type))
               ((or) (eql kind 'union-type))
               ((member) (eql kind 'member-type))
               ((eql) (eql kind 'eql-type)) ; member-type?
               ((satisfies) (eql kind 'satisfies-type))
               ((not) (eql kind 'negation-type))
               (t (subtypecheck (first type)))))
            ((symbolp kind)
             (let ((class (find-class kind nil environment))) ; kind may be, e.g., BUILT-IN-CLASS
               (if class
                   (typep type class)
                   (subtypecheck type))))
            ((typep kind 'class) (typep type kind))
            (t (error "~s is not a kind designator" kind))))))

;;; TODO: ckindcase and/or make this all suck less

;;; case-body may be found in misc.lisp.

(defmacro kindcase (keyform &body cases)
  "As TYPECASE, but with KINDP instead of TYPEP."
  (case-body 'kindcase keyform cases 'kindp nil))

(defmacro ekindcase (keyform &body cases)
  "As ETYPECASE, but with KINDP instead of TYPEP."
  (case-body 'ekindcase keyform cases 'kindp t))
