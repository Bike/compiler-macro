;;;; type-util.lisp
#||
To the extent possible under law, the author(s) have dedicated all copyright and related and
neighboring rights to this software to the public domain worldwide. This software is distributed
without any warranty.

You should have received a copy of the CC0 Public Domain Dedication along with this software. If not,
see <http://creativecommons.org/publicdomain/zero/1.0/>. 
||#

;;;; utility functions for working with type (specifier)s.
;;;; it's fairly ugly but works okay.  see notes/types.txt for wishlisting.
;;;; see also kinds.lisp.

(in-package #:sandalphon.compiler-macro)

(defun function-type-return-type (function-type)
  "Given a function type, return the result type (which may be a values type) of that function type."
  (if (consp function-type)
      (third function-type) ; (FUNCTION args return)
      '*)) ; FUNCTION

(defun values-type-primary (maybe-values-type)
  "Given a values type, return the type of the primary value."
  (cond ((and (consp maybe-values-type) (eql (first maybe-values-type) 'values))
         ;; not the best...
         (dolist (arg (rest maybe-values-type) 'null) ; take care of (values)
           (unless (member arg lambda-list-keywords)
             (return-from values-type-primary arg))))
        ((eql maybe-values-type '*) 't)
        (t maybe-values-type)))

(defun function-type-primary-value (function-type)
  "Given a function type, return the type of the function's primary return value."
  (values-type-primary (function-type-return-type function-type)))

(defun array-type-element-type (array-type)
  "Return the (possibly unupgraded) specialized element-type of the given array type, or *."
  (ecase (if (consp array-type) (first array-type) array-type)
    ((string simple-string) 'character) ; not quite right
    ((simple-vector) 't)
    ((vector array simple-array) (if (consp array-type) (second array-type) '*))
    ((bit-vector simple-bit-vector) 'bit)))

(defun array-type-dimensions (array-type)
  "Return the known dimensions of a given array type.
A dimensions designator is either a positive fixnum, representing a rank; a list of positive fixnums
or the symbol *, representing dimension lengths; or the symbol *, representing no information."
  (if (consp array-type)
      (ecase (first array-type)
        ((string simple-string base-string simple-vector bit-vector simple-bit-vector)
         (if (second array-type) (list (second array-type)) '*))
        ((vector) (if (third array-type) (list (third array-type)) '*))
        ((array simple-array) (or (third array-type) '*)))
      (ecase array-type
        ((array simple-array vector simple-string simple-vector bit-vector simple-bit-vector)
         '*))))

(defun union-type-types (union-type)
  "Return the types the given union (OR) type is composed of."
  (rest union-type))
(defun intersection-type-types (intersection-type)
  "Return the types the given intersection (AND) type is composed of."
  (rest intersection-type))

(defun negation-type-type (negation-type)
  "Return the type the given negation (NOT) type is a negation of."
  (second negation-type))

(defun numeric-type-low (numtype)
  "Given a bounded numeric type (i.e. a scalar, not COMPLEX) return the known lower bound of that type.
A bound designator is either a number, representing an inclusive bound, or a list of a number, 
representing an exclusive bound."
  (if (consp numtype)
      (ecase (first numtype)
        ((mod) 0)
        ((unsigned-byte) 0)
        ((signed-byte) (let ((exp (second numtype)))
                         (if (numberp exp)
                             (- (expt 2 (1- exp)))
                             '*)))
        ((float
          short-float single-float double-float long-float
          real rational integer)
         (second numtype))
        ((ratio) '*))
      (ecase numtype
        ((fixnum) most-negative-fixnum)
        ((unsigned-byte) 0)
        ((float
          short-float single-float double-float long-float
          real rational integer ratio signed-byte)
         '*))))

(defun numeric-type-high (numtype)
  "Given a bounded numeric type (i.e. a scalar, not COMPLEX) return the known upper bound of that type.
A bound designator is either a number, representing an inclusive bound, or a list of a number,
representing an exclusive bound."
  (if (consp numtype)
      (ecase (first numtype)
        ;; could use exclusive bounds (i.e. lists) but ehhhhh
        ((mod) (1- (second numtype)))
        ((unsigned-byte) (1- (expt 2 (second numtype))))
        ((signed-byte) (let ((exp (second numtype)))
                         (if (numberp exp)
                             (1- (expt 2 (1- exp)))
                             '*)))
        ((float
          short-float single-float double-float long-float
          real rational integer)
         (third numtype))
        ((ratio) '*))
      (ecase numtype
        ((fixnum) most-positive-fixnum)
        ((float
          short-float single-float double-float long-float
          real rational integer ratio signed-byte unsigned-byte)
         '*))))

(defun complex-type-element-type (complex-type)
  "Given a complex type, return the (possibly unupgraded) element-type, or *."
  (if (consp complex-type)
      (or (second complex-type) '*)
      '*))

(defun eql-type-object (eqltype)
  "Given an EQL type, return the prototypical object objects of that type are EQL to."
  (second eqltype))

(defun member-type-members (member-type)
  "Given a MEMBER type, return the elements of that type."
  (rest member-type))

(defun satisfies-type-function (satisfies-type)
  "Given a SATISFIES type, return the name of the discriminating function it uses."
  (second satisfies-type))

(defun cons-type-car-type (cons-type)
  "Given a CONS type, return the type its CAR must be, or *."
  (if (consp cons-type)
      (or (second cons-type) '*)
      '*))

(defun cons-type-cdr-type (cons-type)
  "Given a CONS type, return the type its CDR must be, or *."
  (if (consp cons-type)
      (or (third cons-type) '*)
      '*))
