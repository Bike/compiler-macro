;;;; opticl.lisp

;;;; http://cyrusharmon.org/blog/display?id=113 but with this library.

;;;; three versions:
;;;; /opticl is a pretty basic translation of the original.
;;;; /opticl-ecase works by putting a compiler macro on ecase instead, and relying on the implementation for inlining.
;;;; /opticl-simple is supposed to have the same effects, but with the "cleanest" code.  in that it is twenty lines.
;;;; (not counting the utility functions that are more or less part of the library)

;;;; and after that, a few notes.

(defpackage #:sandalphon.compiler-macro-test/opticl
  (:use #:cl #:alexandria #:sandalphon.compiler-macro)
  (:export #:pixel))

(in-package #:sandalphon.compiler-macro-test/opticl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-dcase (get d y x &optional stores)
    (flet ((form (i)
	     (if stores
		 `(setf (aref ,get ,y ,x ,i) ,(elt stores i))
		 `(aref ,get ,y ,x ,i))))
      `(ecase ,d
	 ,@(loop for n from 1 to opticl::+max-image-channels+
	      collect `(,n (values ,@(loop for i from 0 below n collect (form i)))))))))

(declaim (inline pixel))
;; putting this in a macrolet lets changes in the max number of channels (opticl::+max-image-channels+) propagate
(macrolet ((def ()
	     `(defun pixel (image y x)
		(ecase (array-rank image)
		  (2 (aref image y x))
		  (3 ,@(gen-dcase 'image '(array-dimension image 2) 'y 'x))))))
  (def))

(define-compiler-hinter pixel (image y x))

(define-compiler-hint pixel (image y x &environment env)
    "Fold rank/dimension discrimination for arrays of known rank/dimension"
  (let* ((dims (array-type-dimensions-or-give-up (form-type image env)))
	 (rank (if (integerp dims) dims (length dims))))
    (ecase rank
      (2 `(aref ,image ,y ,x))
      (3 (cond ((and (listp dims) (integerp (third dims)))
		(assert (<= 1 (third dims) opticl::+max-image-channels+)) ; FIXME: should be a warning, not an error
		(once-only (image y x)
		  `(values ,@(loop for i below (third dims) collect `(aref ,image ,y ,x ,i)))))
	       (t (note-optimization-failure "length of third dimension of ~s couldn't be determined at compile-time"
					     image)
		  (once-only (image y x) (gen-dcase image `(array-dimension ,image 2) y x)))))
      ((*) (decline-expansion "rank or length of third dimension of ~s couldn't be determined at compile-time" image)))))

(defun derive-array-dimensions (type)
  (labels ((maybe (type)
	     (kindcase type
	       (array-type (array-type-dimensions type)))))
    (or (maybe type) '*)))

(define-setf-expander pixel (image y x &environment env)
  ;; doing compiler macroy stuff in a setf expansion seems a bit weird, but it's compile-time anyway, right?
  (flet ()
    (multiple-value-bind (dummies vals newval setter getter)
	(get-setf-expansion image env)
      (declare (ignore newval setter))
      (let* ((dims (derive-array-dimensions getter))
	     (rank (etypecase dims
		     ((eql *) dims)
		     (list (length dims))
		     (integer dims))))
	(ecase rank
	  ((2) (let ((temp-y (gensym "Y")) (temp-x (gensym "X")) (store (gensym)))
		 (values `(,@dummies ,temp-y ,temp-x)
			 `(,@vals ,y ,x)
			 `(,store)
			 `(setf (aref ,getter ,temp-y ,temp-x) ,store)
			 `(aref ,getter ,temp-x ,temp-y))))
	  ((3)
	   (let ((arity (or (and (listp dims)
				 (= (length dims) 3)
				 (third dims))
			    '*)))
	     (cond ((integerp arity)
		    (assert (<= 1 arity opticl::+max-image-channels+)) ; FIXME: should be a warning, not an error
		    (let* ((stores (map-into (make-list arity) #'gensym))
			   (temp-y (gensym "Y")) (temp-x (gensym "X"))
			   (temp-image (gensym "IMAGE"))
			   (arefs (loop for i from 0 below arity collecting `(aref ,temp-image ,temp-y ,temp-x ,i))))
		      (values `(,temp-image ,temp-y ,temp-x)
			      `(,getter ,y ,x)
			      stores
			      `(setf (values ,@arefs)
				     (values ,@stores))
			      `(values ,@arefs))))
		   (t (note-optimization-failure "number of channels of ~s not determinable at compile-time, ~
                                                  doing runtime check"
						 getter)
		      (let ((syms (map-into (make-list opticl::+max-image-channels+) #'gensym))
			    (temp-image (gensym "IMAGE"))
			    (temp-y (gensym "Y")) (temp-x (gensym "X")))
			(values `(,temp-image ,temp-y ,temp-x)
				`(,getter ,y ,x)
				syms
				(gen-dcase temp-image `(array-dimension ,temp-image 2) temp-y temp-x)
				(gen-dcase temp-image `(array-dimension ,temp-image 2) temp-y temp-x syms)))))))
	  ((*)
	   (note-optimization-failure "couldn't determine dimensions of ~s, have to do runtime check" getter)
	   (let ((syms (map-into (make-list opticl::+max-image-channels+) #'gensym))
		 (temp-image (gensym "IMAGE"))
		 (temp-d (gensym "D"))
		 (temp-y (gensym "Y"))
		 (temp-x (gensym "X")))
	     (values `(,temp-image ,temp-y ,temp-x ,temp-d)
		     `(,image ,y ,x (array-dimension ,temp-image 2))
		     syms
		     `(ecase (array-rank ,temp-image)
			(2 (setf (aref ,temp-image ,temp-y ,temp-x) ,(elt syms 0)))
			(3 ,(gen-dcase temp-image temp-d temp-y temp-x syms)))
		     `(ecase (array-rank ,temp-image)
			(2 (aref ,temp-image ,temp-y ,temp-x) ,(elt syms 0))
			(3 ,(gen-dcase temp-image temp-d temp-y temp-x)))))))))))

;;; but that's, well, very long.  how about instead we just help the compiler out with ecase directly?

(defpackage #:sandalphon.compiler-macro-test/opticl-ecase
  (:use #:cl #:alexandria #:sandalphon.compiler-macro)
  (:shadow #:ecase)
  (:export #:pixel))

(in-package #:sandalphon.compiler-macro-test/opticl-ecase)

;;; we can't (conformingly) define compiler macros on CL:ECASE, so we'll do this to get around that

(defmacro ecase (keyform &body cases)
  `(cl:ecase ,keyform ,@cases))

;; have to do this again, since the other package's version uses CL:ECASE
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-dcase (get d y x &optional stores)
    (flet ((form (i)
	     (if stores
		 `(setf (aref ,get ,y ,x ,i) ,(elt stores i))
		 `(aref ,get ,y ,x ,i))))
      `(ecase ,d
	 ,@(loop for n from 1 to opticl::+max-image-channels+
	      collect `(,n (values ,@(loop for i from 0 below n collect (form i)))))))))

;;; now set up ecase to be optimized.

(define-compiler-hinter ecase (keyform &body cases))

(defun array-type-dimensions-or-give-up (type)
  (labels ((maybe-array-type-dimensions (type)
             (kindcase type
               (array-type
                (array-type-dimensions type))
               (union-type
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (union-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (decline-expansion
                       "~@<dimensions of arrays in union type ~S do not match~:@>"
                       #+(or)(type-specifier type) type)))))
               (intersection-type
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (intersection-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (abort-expansion 
                       "~@<dimensions of arrays in intersection type ~S do not match~:@>"
                       #+(or)(type-specifier type) type))))))))
    (or (maybe-array-type-dimensions type)
        (decline-expansion
         "~@<don't know how to extract array dimensions from type ~S~:@>"
         #+(or)(type-specifier type) type))))

;;; i'd rather not rely on loking at literal calls, but what can ya do (for now)

(define-compiler-hint ecase (keyform &body cases &environment env)
    "skip array-rank cases if possible"
  (unless (and (consp keyform) (eq (first keyform) 'array-rank)) (decline-expansion))
  (let* ((dims (array-type-dimensions-or-give-up (form-type (second keyform) env)))
	 (rank (if (listp dims) (length dims) dims)))
    ;; TODO: code deletion notes? (for the cases not taken)
    `(progn
       ,(second keyform) ; perform any side effects snuck into the keyform
       ,@(block nil
	   (mapc (lambda (caseform)
	  	 (when (if (listp (first caseform))
	  		   (member rank (first caseform))
			   (eql rank (first caseform)))
		   (return (rest caseform))))
	       cases)
	 ;; wow bad
	 (error "no applicable clause in ~s" 'ecase)))))

(define-compiler-hint ecase (keyform &body cases &environment env)
    "skip array-dimension cases for constant dimension, if possible"
  (unless (and (consp keyform) (eq (first keyform) 'array-dimension)) (decline-expansion))
  (unless (and (= (length keyform) 3) (constantp (third keyform)))
    ;; could be cleverer here, e.g. if the axis is within some bounds?  probably not worth it
    (decline-expansion "axis referenced by ~s in ~s could not be determined" keyform 'ecase))
  (let* ((dims (array-type-dimensions-or-give-up (form-type (second keyform) env)))
	 (axis (eval (third keyform))) ; constant-form-value, basically
	 (dim (if (listp dims)
		  (if (> (length dims) axis)
		      (nth axis dims)
		      (abort-expansion "found ~s, when ~s has ~d dimensions" keyform (second keyform) (length dims)))
		  '*)))
    (when (eq '* dim)
      (decline-expansion "~dth dimension of ~s could not be determined" axis (second keyform)))
    `(progn
       ,(second keyform) ; side effects
       ,@(block nil
	 (mapc (lambda (caseform)
		 (when (if (listp (first caseform))
			   (member dim (first caseform))
			   (eql dim (first caseform)))
		   (return (rest caseform))))
	       cases)
	 ;; still bad
	 (error "no applicable clause in ~s" 'ecase)))))

;;; now that all the macros are ready, just write pixel in the mostly naïve way.

(declaim (inline pixel))
(macrolet ((def ()
	     `(progn
		(defun pixel (image y x)
		  (ecase (array-rank image)
		    (2 (aref image y x))
		    (3 ,(gen-dcase 'image '(array-dimension image 2) 'y 'x)))))))
  (def))

(define-setf-expander pixel (image y x &environment env)
  (multiple-value-bind (dummies vals newvals setter getter)
      (get-setf-expansion image env)
    (declare (ignore dummies vals newvals setter))
    (let ((_image (gensym "IMAGE")) (_y (gensym "Y")) (_x (gensym "X"))
	  (stores (map-into (make-list opticl::+max-image-channels+) #'gensym)))
      (values 
       `(,_image ,_y ,_x)
       `(,getter ,y ,x)
       stores
       `(ecase (array-rank ,_image)
	  (2 (setf (aref ,_image ,_y ,_x) ,(first stores)))
	  (3 ,(gen-dcase _image `(array-dimension ,_image 2) _y _x stores)))
       `(ecase (array-rank ,_image)
	  (2 (aref ,_image ,_y ,_x) ,(first stores))
	  (3 ,(gen-dcase _image `(array-dimension ,_image 2) _y _x)))))))

;; or we could just forget the case entirely.  for dimensions, at least.  I think this results in some cleaner code.

(defpackage #:sandalphon.compiler-macro-test/opticl-simple
  (:use #:cl #:alexandria #:sandalphon.compiler-macro)
  (:export #:pixel))

;; eliding setf expansion this time - this is getting repetitive and this should be illustrative enough

(declaim (inline pixel))
(defun pixel (image y x)
  (ecase (array-rank image)
    (2 (aref image y x))
    (3 (values-list (loop for i from 0 below (array-dimension image 2) collect (aref image y x i))))))

(define-compiler-hinter pixel (image y x))
(define-compiler-hint pixel (image y x &environment env)
    "yet more partial inlining (for known rank 2 images)"
  (unless (form-typep image '(array * 2) env) (decline-expansion))
  `(aref ,image ,y ,x))

(define-compiler-hint pixel (&whole form image y x &environment env)
    "rank three"
  (let* ((dims (array-type-dimensions-or-give-up (form-type image env)))
	 (dim (or (third dims) (decline-expansion)))) ; not of rank three
    (when (eq dim '*) (decline-expansion "could not determine number of channels in ~s - ~s may cons" image form))
    (once-only (image y x)
      `(values ,@(loop for i from 0 below dim collect `(aref ,image ,y ,x ,i))))))

#||

And that's that:

> (compiler-macroexpand '(pixel foo 4 3))

; STYLE-WARNING: don't know how to extract array dimensions from type T
(PIXEL FOO 4 3)
NIL

> (compiler-macroexpand '(pixel (the (simple-array fixnum (* * 9)) foo) 4 3))

(LET ((#:IMAGE1517 (THE (SIMPLE-ARRAY FIXNUM (* * 3)) FOO))
      (#:Y1518 4)
      (#:X1519 3))
  (VALUES (AREF #:IMAGE1517 #:Y1518 #:X1519 0)
          (AREF #:IMAGE1517 #:Y1518 #:X1519 1)
          (AREF #:IMAGE1517 #:Y1518 #:X1519 2)))
T

> (disassemble (lambda (foo)
		 (declare (optimize speed (safety 0)))
		 (pixel (the (simple-array fixnum (* * 3)) foo) 4 3)))
; disassembly for (LAMBDA (FOO))
; 0D28EADA:       8B421D           MOV EAX, [EDX+29]          ; no-arg-parsing entry point
;      ADD:       C1E002           SHL EAX, 2
;      AE0:       83C00C           ADD EAX, 12
;      AE3:       8D0440           LEA EAX, [EAX+EAX*2]
;      AE6:       8BC8             MOV ECX, EAX
;      AE8:       8B4209           MOV EAX, [EDX+9]
;      AEB:       8B5C0801         MOV EBX, [EAX+ECX+1]
;      AEF:       8B421D           MOV EAX, [EDX+29]
;      AF2:       C1E002           SHL EAX, 2
;      AF5:       83C00C           ADD EAX, 12
;      AF8:       8D0440           LEA EAX, [EAX+EAX*2]
;      AFB:       8D4804           LEA ECX, [EAX+4]
;      AFE:       8B4209           MOV EAX, [EDX+9]
;      B01:       8B7C0801         MOV EDI, [EAX+ECX+1]
;      B05:       8B421D           MOV EAX, [EDX+29]
;      B08:       C1E002           SHL EAX, 2
;      B0B:       83C00C           ADD EAX, 12
;      B0E:       8D0440           LEA EAX, [EAX+EAX*2]
;      B11:       8D4808           LEA ECX, [EAX+8]
;      B14:       8B4209           MOV EAX, [EDX+9]
;      B17:       8B740801         MOV ESI, [EAX+ECX+1]
;      B1B:       8BD3             MOV EDX, EBX
;      B1D:       8D5D08           LEA EBX, [EBP+8]
;      B20:       B90C000000       MOV ECX, 12
;      B25:       F9               STC
;      B26:       8BE5             MOV ESP, EBP
;      B28:       5D               POP EBP
;      B29:       C3               RET
NIL

Which is exactly the same disassembly as generated with opticl:pixel.  But now the definition is a bit clearer/modular/I like it more; or more concretely, PIXEL is now a normal function, that can be funcalled/applied/etc. with some loss of efficiency.  Having the function definition include the ECASE partial inlining would make such un-compiler-macro'd calls more efficient (having only to branch at runtime, given a sufficiently small channel count.)

Of course there could be improvements.  "don't know how to extract array dimensions from type T" isn't very helpful, so maybe it would be nice to rewrite array-type-dimensions-or-give-up to print something like "insufficient declaration information to optimize based on type of form ~s".  Or derive-array-type-dimensions could be used, and hints could define their own errors.  Or error checking in general could be improved, more type util stuff, more macros for less (or [...] (error ...)) crap.

Warning conditions could be classes instead of defaulting to simple-optimization-note, so a sufficiently irritated programmer could (handler-bind ((optimization-type-warning #'muffle-warning)) (asdf:compile-system ....)) [does that even work?  i doubt it] without necessarily losing other information they may care more about.

Something like (but quite different from, really) generic functions would be nice syntactically and otherwise.  E.g., the rank 2 case would be written as

(define-compiler-method pixel ((image (array * 2)) y x)
  "yet more partial inlining (for known rank 2 images)"
  `(aref ,image ,y ,x))

Here the string is a regular docstring, not a qualifier (not something i really like anyway) since the types handle uniqueness, like they mostly do with real methods.

Imagining further, we could have pattern matching, allowing the third case to be (with some added declarations because why not)

(define-compiler-method pixel (&whole form (image (array * (* * dim))) y x &environment env)
  "rank three"
  (when (or (> n 9) ; don't bother if there are enough channels to overflow registers anyway (9 is arbitrary here)
	    (policy (> space speed) env)) ; probably doesn't actually take up too much space, but eh
    (decline-expansion))
  (once-only (image y x)
    `(values ,@(loop for i from 0 below dim collect `(aref ,image ,y ,x ,i)))))

This meaning that the expansion should only occur if image is known to be a rank three array, and further that the third dimension must be known, and that it is bound as DIM within the body of the method.  (alternately it could catch * as well, requiring a check in the body?  eh)

(also note that &whole and &environment are still available.)

Bla bla method combinations other CLOS goodness who knows.

Other speculative notes are in notes/

||#
