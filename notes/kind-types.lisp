
;;; types for type specifiers
;;; turns out they kind of suck!

(deftype optional-kind (cars &rest types)
  ;; modification means no backquote (literal data)
  (let* ((cars (if (listp cars) cars (list cars)))
	 (res (list 'or (list* 'member cars) (list 'cons (list* 'member cars) 'null)))
	 (mod (cdr (third res))))
    (dolist (type types res)
      (setf (second mod) (list 'or 'null (list 'cons (list 'or '(eql *) type) 'null))
	    mod (cdr (third (second mod)))))))

;;; FIXME: Causes a (spurious?) style-warning in SBCL, relating to supposedly not using ENV.
;;; ditto for later kind defs, probably (except no, it doesn't for array-type...) (but it does for complex.)
;;; looks like this is due to a compiler-macro on find-class.  ironic?
(deftype cons-type (&environment env)
  #+sbcl (declare (ignorable env))
  `(or (optional-kind cons type type)
       (eql ,(find-class 'cons t env))))

(deftype array-type (&environment env)
  `(or (optional-kind (array simple-array) type (or (integer 0 (#.array-rank-limit)) list))
       (optional-kind (vector) type (integer 0 (#.array-dimension-limit)))
       (optional-kind (simple-vector string base-string simple-string simple-base-string
				     bit-vector simple-bit-vector)
		      (integer 0 (#.array-dimension-limit)))
       ;; note that we're referring to the class objects themselves here.
       (member
	;; system classes
	,@(mapcar (lambda (name) (find-class name t env))
		  '(array vector string bit-vector))
	;; types, which may or may not have associated classes
	,@(delete nil
		  (mapcar (lambda (name) (find-class name nil env))
			  '(simple-vector base-string simple-string simple-base-string
			    bit-vector simple-bit-vector))))))

(deftype complex-type (&environment env)
  #+sbcl (declare (ignorable env))
  `(or (optional-kind complex type)
       (eql ,(find-class 'complex t env))))

(deftype numeric-type (&environment env)
  (flet ((bounded (type)
	   `(optional-kind ,type (or (cons ,type null) ,type) (or (cons ,type null) ,type))))
    `(or complex-type
	 (eql ratio)
	 ,@(mapcar #'bounded
		   '(float short-float single-float double-float long-float
		     integer rational real))
	 (eql number)
	 (member bit fixnum bignum signed-byte unsigned-byte)
	 (cons (member unsigned-byte signed-byte) (cons (integer 0) null))
	 (cons (member mod) (cons (integer 0) null))
	 ;; again, using the classes themselves
	 (member
	  ;; CL classes
	  ,@(mapcar (lambda (name) (find-class name t env))
		    '(number real float rational ratio integer))
	  ;; possible classes
	  ,@(delete nil
		    (mapcar (lambda (name) (find-class name nil env))
			    '(short-float single-float double-float long-float
			      unsigned-byte signed-byte bit fixnum bignum)))))))

(deftype intersection-type () `(cons (eql and)))
(deftype union-type () `(cons (eql or)))
(deftype compound-type () '(or intersection-type union-type))

(deftype negation-type () `(cons (eql not) (cons type null)))

(deftype satisfies-type () `(cons (eql satisfies) (cons symbol null)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; it's only used as a documentation specifier anyway...
  (shadow 'cl:type))
(deftype type (&environment env)
  `(or numeric-type cons-type array-type
       compound-type negation-type satisfies-type
       ,(find-class 'class t env)))